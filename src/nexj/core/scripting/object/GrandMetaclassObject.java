// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;

/**
 * Class object of the metaclass object.
 */
public class GrandMetaclassObject extends MetaclassObject
{
   // constants

   /**
    * The grand metaclass singleton.
    */
   public final static GrandMetaclassObject INSTANCE = new GrandMetaclassObject();

   // constructors

   /**
    * Constructs the grand metaclass object.
    * @param grandMetaclass The metaclass of the grand metaclass.
    * @param symbol The symbol of the constructed object.
    */
   protected GrandMetaclassObject(MetaclassObject grandMetaclass, Symbol symbol)
   { 
      super(grandMetaclass, symbol);
      m_class = null;

      ClassObject.addMembers(this);

      addMethod(":new", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            Object symbol = machine.getArg(1, nArgCount);

            machine.returnValue(((MetaclassObject)machine.getArg(0, nArgCount))
               .createClass((symbol == null || symbol instanceof Symbol) ? (Symbol)symbol :
                  Symbol.define((String)symbol)), nArgCount);

            return false;
         }
      });

      addMethod(":define", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            Object obj = machine.getArg(1, nArgCount);

            if (obj instanceof ClassObject)
            {
               getEnvironment().defineClass((ClassObject)obj);
               machine.returnValue(obj, nArgCount);
            }
            else
            {
               machine.returnValue(((MetaclassObject)machine.getArg(0, nArgCount))
                  .defineClass((obj == null || obj instanceof Symbol) ? (Symbol)obj :
                     Symbol.define((String)obj)), nArgCount);
            }

            return false;
         }
      });

      m_class = this;
   }

   /**
    * Constructs the grand metaclass object.
    */
   protected GrandMetaclassObject()
   {
      this(GrandMetaclassObject.INSTANCE, Symbol.define("sys:GrandMetaclass"));
   }

   // operations

   /**
    * @see nexj.core.scripting.object.MetaclassObject#setValue(nexj.core.scripting.object.ClassObject, int, java.lang.Object, Machine)
    */
   protected void setValue(ClassObject classObject, int nOffset, Object value, Machine machine)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getValue(nexj.core.scripting.object.ClassObject, int, Machine)
    */
   protected Object getValue(ClassObject classObject, int nOffset, Machine machine)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createInstance(ClassObject)
    */
   protected ObjectOriented createInstance(ClassObject classObject)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createMetaclass(nexj.core.scripting.Symbol)
    */
   protected MetaclassObject createMetaclass(Symbol symbol)
   {
      throw new UnsupportedOperationException();
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
   protected MethodObject createMethod(ClassObject classObject, Symbol symbol,
      int nArgCount, boolean bVarArg, Function function)
   {
      return new MethodObject(classObject, symbol, nArgCount, bVarArg, function);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateBase(nexj.core.scripting.object.ClassObject)
    */
   protected void validateBase(ClassObject base)
   {
      // Prevents modification of the grand metaclass once initialized by the constructor
      if (m_class != null && base instanceof GrandMetaclassObject)
      {
         throw new UnsupportedOperationException();
      }
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateAttribute(nexj.core.scripting.object.ClassObject, nexj.core.scripting.object.AttributeObject)
    */
   protected void validateAttribute(ClassObject classObject, AttributeObject attribute)
   {
      // Prevents modification of the grand metaclass once initialized by the constructor
      if (m_class != null && classObject instanceof GrandMetaclassObject)
      {
         throw new UnsupportedOperationException();
      }
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#validateMethod(nexj.core.scripting.object.ClassObject, nexj.core.scripting.object.MethodObject)
    */
   protected void validateMethod(ClassObject classObject, MethodObject method)
   {
      // Prevents modification of the grand metaclass once initialized by the constructor
      if (m_class != null && classObject instanceof GrandMetaclassObject)
      {
         throw new UnsupportedOperationException();
      }
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
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#addObjectMembers(nexj.core.scripting.object.ClassObject)
    */
   protected void addObjectMembers(ClassObject classObject)
   {
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getObjectClassSymbol()
    */
   protected Symbol getObjectClassSymbol()
   {
      return BasicMetaclassObject.METACLASS_SYMBOL;
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getObjectBaseClassSymbol()
    */
   protected Symbol getObjectBaseClassSymbol()
   {
      return null;
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#define()
    */
   protected void define()
   {
   }
}
