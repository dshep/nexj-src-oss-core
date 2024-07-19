// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Class object of a class object.
 * The metaclass is a strategy object of the object system,
 * i.e. it is used to customize the metaobject protocol.
 */
public abstract class MetaclassObject extends ClassObject
{
   // constructors

   /**
    * Constructs the metaclass.
    * @param symbol The metaclass symbol.
    */
   public MetaclassObject(Symbol symbol)
   {
      super(GrandMetaclassObject.INSTANCE, symbol);
   }

   /**
    * Constructs the metaclass with a custom grand metaclass.
    * @param grandMetaclass The class of this metaclass.
    * @param symbol The metaclass symbol.
    */
   public MetaclassObject(MetaclassObject grandMetaclass, Symbol symbol)
   {
      super(grandMetaclass, symbol);
   }

   // operations

   /**
    * Sets an attribute value value on a given class object.
    * @see nexj.core.scripting.object.GenericObject#setValue(int, java.lang.Object, Machine)
    */
   protected abstract void setValue(ClassObject classObject, int nOffset, Object value, Machine machine);

   /**
    * Gets an attribute value from a given class object.
    * @see nexj.core.scripting.object.GenericObject#getValue(int, Machine)
    */
   protected abstract Object getValue(ClassObject classObject, int nOffset, Machine machine);

   /**
    * Creates an object instance corresponding to a class object.
    * This is different from createObject(), which creates a class object.
    * @param classObject The class object.
    */
   protected abstract ObjectOriented createInstance(ClassObject classObject);

   /**
    * Creates a new instance of the same metaclass.
    * @param symbol The metaclass symbol.
    * @return The created metaclass object. 
    */
   protected abstract MetaclassObject createMetaclass(Symbol symbol);

   /**
    * Creates an attribute object.
    * This is invoked both for class objects and metaclass objects.
    * @param classObject The class object.
    * @param symbol The attribute symbol.
    * @return The attribute object.
    */
   protected abstract AttributeObject createAttribute(ClassObject classObject, Symbol symbol);

   /**
    * Creates a method object.
    * This is invoked both for class objects and metaclass objects.
    * @param classObject The class object.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    * @param bVarArg True to indicate that the last argument is of variable length.
    * @param function The implementation function. 
    * @return The method object.
    */
   protected abstract MethodObject createMethod(ClassObject classObject,
      Symbol symbol, int nArgCount, boolean bVarArg, Function function);

   /**
    * Validates a base class object.
    * This is invoked both for class objects and metaclass objects.
    */
   protected abstract void validateBase(ClassObject base);

   /**
    * Validates an attribute.
    * This is invoked both for class objects and metaclass objects.
    * @param classObject The class object.
    */
   protected abstract void validateAttribute(ClassObject classObject, AttributeObject attribute);

   /**
    * Validates a method.
    * This is invoked both for class objects and metaclass objects.
    * @param classObject The class object.
    */
   protected abstract void validateMethod(ClassObject classObject, MethodObject method);

   /**
    * Sorts the attribute offsets according to their initializer.
    * @param classObject The class object.
    * @param offsetArray The array to sort, initially sorted by offset.
    */
   protected abstract void sortInitializers(ClassObject classObject, int[] offsetArray);

   /**
    * Resolves a class object.
    * This is invoked both for class objects and metaclass objects.
    * @param classObject The class object to resolve.
    */
   protected abstract void resolve(ClassObject classObject);

   /**
    * Adds object members to the class object.
    * @param classObject The destination class object.
    */
   protected abstract void addObjectMembers(ClassObject classObject);

   /**
    * @return The root object class symbol.
    */
   protected abstract Symbol getObjectClassSymbol();

   /**
    * @return The root object base class symbol. Can be null.  
    */
   protected abstract Symbol getObjectBaseClassSymbol();

   /**
    * Creates an anonymous class object.
    * @see nexj.core.scripting.object.ClassObject#createObject()
    */
   public ObjectOriented createObject()
   {
      return createClass(null);
   }

   /**
    * @see nexj.core.scripting.object.ClassObject#isMetaclass()
    */
   public boolean isMetaclass()
   {
      return true;
   }

   /**
    * @see nexj.core.scripting.object.ClassObject#getMetaclass()
    */
   public MetaclassObject getMetaclass()
   {
      return this;
   }

   /**
    * Creates a class object.
    * @param symbol The class symbol. Can be null.
    * @return The created class object.
    */
   public ClassObject createClass(Symbol symbol)
   {
      return new ClassObject(createMetaclass(symbol), symbol);
   }

   /**
    * Defines a class with a given symbol.
    * @param symbol The class symbol.
    * @return The defined class object.
    */
   public ClassObject defineClass(Symbol symbol)
   {
      ClassEnvironment env = getEnvironment();
      ClassObject classObject = env.findClass(symbol);

      if (classObject == null)
      {
         classObject = createClass(symbol);
         classObject.setForward(true);
         env.defineClass(classObject);
      }

      return classObject;
   }

   /**
    * Defines the root class object.
    */
   protected void define()
   {
      ClassEnvironment env = getEnvironment();

      env.defineClass(this);

      ClassObject classObject = defineClass(getObjectClassSymbol());

      if (classObject.isForward())
      {
         classObject.setForward(false);
         classObject.getMetaclass().addBase(this);

         Symbol baseSymbol = getObjectBaseClassSymbol();

         if (baseSymbol != null)
         {
            classObject.addBase(env.findClass(baseSymbol));
         }

         addObjectMembers(classObject);
      }
   }
}
