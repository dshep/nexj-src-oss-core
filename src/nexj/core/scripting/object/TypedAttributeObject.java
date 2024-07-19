package nexj.core.scripting.object;

import nexj.core.meta.AttributeTypeMismatchException;
import nexj.core.meta.Type;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.TypedMetaclassObject.TypeErrorHandler;

/**
 * Typed attribute implementation.
 */
public class TypedAttributeObject extends AttributeObject implements TypeErrorHandler
{
   // constants

   /**
    * The typed attribute class object symbol.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:TypedAttribute");

   // attributes

   /**
    * True if the attribute takes a collection; false otherwise.
    */
   protected boolean m_bCollection;

   /**
    * True if the attribute must be non-null during RPC.
    */
   protected boolean m_bRequired;

   /**
    * True to derive type mappings from equivalent attribute in the base class.
    */
   protected boolean m_bDerived;

   // associations

   /**
    * The attribute value type.
    */
   protected Type m_type;

   // constructors

   /**
    * Constructs the typed attribute.
    * @param classObject The attribute class object. This is not the holder.
    * @param holder The holder class. Can be null.
    * @param symbol The attribute symbol.
    */
   public TypedAttributeObject(ClassObject classObject, ClassObject holder, Symbol symbol)
   {
      super(classObject, holder, symbol);
   }

   /**
    * Constructs the typed attribute.
    * @param holder The holder class. Can be null.
    * @param symbol The attribute symbol.
    */
   public TypedAttributeObject(ClassObject holder, Symbol symbol)
   {
      super(ClassObject.getEnvironment().findClass(CLASS_SYMBOL), holder, symbol);
   }

   // operations

   /**
    * Sets the attribute value type.
    * @param type The type of the data stored in the attribute value.
    */
   public void setValueType(Type type)
   {
      change();
      m_type = type;
   }

   /**
    * Gets the attribute value type.
    * @return The type of the data stored in the attribute value.
    */
   public Type getValueType()
   {
      derive();

      return m_type;
   }

   /**
    * Sets whether the attribute value is a collection.
    * @param bCollection True if the attribute takes a collection; false otherwise.
    */
   public void setCollection(boolean bCollection)
   {
      change();
      m_bCollection = bCollection;
   }

   /**
    * Gets whether the attribute value is a collection.
    * @return True if the attribute takes a collection; false otherwise.
    */
   public boolean isCollection()
   {
      derive();

      return m_bCollection;
   }

   /**
    * Sets the derived flag.
    * @param bDerived True to derive type mappings from equivalent attribute in the base class.
    */
   public void setDerived(boolean bDerived)
   {
      change();
      m_bDerived = bDerived;
   }

   /**
    * Gets the derived flag.
    * @return True to derive type mappings from equivalent attribute in the base class.
    */
   public boolean isDerived()
   {
      return m_bDerived;
   }

   /**
    * Gets whether the attribute is required during RPC.
    * @return True if the attribute must be non-null during RPC.
    */
   public boolean isRequired()
   {
      derive();

      return m_bRequired;
   }

   /**
    * Sets whether the attribute is required during RPC.
    * @param bRequired True if the attribute must be non-null during RPC.
    */
   public void setRequired(boolean bRequired)
   {
      change();
      m_bRequired = bRequired;
   }

   /**
    * Derives type from the equivalent attribute in the base class.
    */
   protected void derive()
   {
      if (m_bDerived)
      {
         assert !m_holder.isForward();

         AttributeObject baseAttr = null;

         for (int nBase = 0, nCount = m_holder.getBaseCount(); nBase < nCount && baseAttr == null; nBase++)
         {
            ClassObject base = m_holder.getBase(nBase);

            baseAttr = base.findAttribute(getSymbol());
         }

         if (baseAttr instanceof TypedAttributeObject)
         {
            TypedAttributeObject typed = (TypedAttributeObject)baseAttr;

            m_type = typed.getValueType();
            m_bCollection = typed.isCollection();
            m_bRequired = typed.isRequired();
         }

         m_bDerived = false;
      }
   }

   /**
    * Exposes members from TypedAttributeObject.
    * Since sys:TypedAttribute is derived from sys:Attribute, the inherited methods are added
    * automatically.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      classObject.addMethod(":type", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            machine.returnValue(attribute.getValueType(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":type", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);
            Object typeSpec = machine.getArg(1, nArgCount);

            attribute.setValueType(TypedMetaclassObject.getForwardType((Symbol)typeSpec, attribute, machine));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":collection?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            machine.returnValue(Boolean.valueOf(attribute.isCollection()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":collection", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            attribute.setCollection(Intrinsic.isTrue(machine.getArg(1, nArgCount)));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":required?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            machine.returnValue(Boolean.valueOf(attribute.isRequired()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":required", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            attribute.setRequired(Intrinsic.isTrue(machine.getArg(1, nArgCount)));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":derived", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedAttributeObject attribute = (TypedAttributeObject)machine.getArg(0, nArgCount);

            attribute.setDerived(Intrinsic.isTrue(machine.getArg(1, nArgCount)));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });
   }

   /**
    * @see nexj.core.scripting.object.TypedMetaclassObject.TypeErrorHandler#error(java.lang.String, java.lang.String, java.lang.Object)
    */
   public void error(String sExpected, String sActual, Object location)
   {
      throw new AttributeTypeMismatchException(this, location, sExpected, sActual);
   }
}
