package nexj.core.scripting.object;

import nexj.core.meta.ArgumentTypeMismatchException;
import nexj.core.meta.Primitive;
import nexj.core.meta.ResultTypeMismatchException;
import nexj.core.meta.Type;
import nexj.core.scripting.Function;
import nexj.core.scripting.Intrinsic;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.scripting.object.TypedMetaclassObject.TypeErrorHandler;

/**
 * Typed method implementation.
 */
public class TypedMethodObject extends MethodObject
{
   // constants

   /**
    * The typed method class object symbol.
    */
   public final static Symbol CLASS_SYMBOL = Symbol.define("sys:TypedMethod");

   /**
    * The :validate-args symbol.
    */
   public final static Symbol VALIDATE_ARGS = Symbol.define(":validate-args");

   /**
    * The :validate-result symbol.
    */
   public final static Symbol VALIDATE_RESULT = Symbol.define(":validate-result");

   // attributes

   /**
    * True if the method returns a collection; false otherwise.
    */
   protected boolean m_bCollection;

   /**
    * True to derive type mappings from equivalent method in the base class.
    */
   protected boolean m_bDerived;

   // associations

   /**
    * The type of each method argument (last position is vararg type, if it is variable-argumented)
    */
   protected Type[] m_argTypeArray;

   /**
    * Flag for each method argument to indicate if it takes a collection.
    */
   protected boolean[] m_argCollectionArray;

   /**
    * The method return value type.
    */
   protected Type m_type;

   /**
    * Handles type mismatch errors in the result.
    */
   protected final TypeErrorHandler RESULT_HANDLER = new TypeErrorHandler()
   {
      public void error(String sExpected, String sActual, Object location)
      {
         throw new ResultTypeMismatchException(TypedMethodObject.this, location, sExpected, sActual);
      }
   };

   /**
    * Handles type mismatch errors in the arguments.
    */
   protected final TypeErrorHandler ARGUMENT_HANDLER = new TypeErrorHandler()
   {
      public void error(String sExpected, String sActual, Object location)
      {
         throw new ArgumentTypeMismatchException(TypedMethodObject.this, location, sExpected, sActual);
      }
   };

   // constructors

   /**
    * Constructs the typed method.
    * @param classObject The method class object.
    * @param holder The method holder. Can be null.
    * @param symbol The method symbol.
    * @param nArgCount The argument count.
    * @param bVarArg True if the method supports variable argument count,
    * in which case the argument values are stored as a pair list in the last argument.
    * @param function The implementation function.
    */
   public TypedMethodObject(ClassObject classObject, ClassObject holder, Symbol symbol, int nArgCount, boolean bVarArg,
      Function function)
   {
      super(classObject, holder, symbol, nArgCount, bVarArg, function);
      m_argTypeArray = new Type[nArgCount];
      m_argCollectionArray = new boolean[nArgCount];
   }

   /**
    * Constructs the typed method.
    * @see TypedMethodObject#TypedMethodObject(ClassObject, ClassObject, Symbol, int, boolean, Function)
    */
   public TypedMethodObject(ClassObject holder, Symbol symbol, int nArgCount, boolean bVarArg, Function function)
   {
      this(ClassObject.getEnvironment().findClass(CLASS_SYMBOL), holder, symbol, nArgCount, bVarArg, function);
   }

   // operations

   /**
    * Gets the method result type.
    * @return The method return value type.
    */
   public Type getResultType()
   {
      derive();

      return m_type;
   }

   /**
    * Sets the method result type.
    * @param type The method return value type.
    */
   public void setResultType(Type type)
   {
      change();
      m_type = type;
   }

   /**
    * Gets whether the return value is a collection.
    * @return True if the method returns a collection; false otherwise.
    */
   public boolean isCollection()
   {
      derive();

      return m_bCollection;
   }

   /**
    * Sets whether the return value is a collection.
    * @param bCollection True if the method returns a collection; false otherwise.
    */
   public void setCollection(boolean bCollection)
   {
      change();
      m_bCollection = bCollection;
   }

   /**
    * Gets the type of the argument.
    * @param nOrdinal The zero-based ordinal of the argument.
    * @return The type of the data passed in the argument.
    */
   public Type getArgType(int nOrdinal)
   {
      derive();

      return m_argTypeArray[nOrdinal];
   }

   /**
    * Sets the type of the argument.
    * @param nOrdinal The zero-based ordinal of the argument.
    * @param type The type of the data passed in the argument.
    */
   public void setArgType(int nOrdinal, Type type)
   {
      change();
      m_argTypeArray[nOrdinal] = type;
   }

   /**
    * Sets whether the argument takes a collection.
    * @param nOrdinal The zero-based ordinal of the argument.
    * @param bCollection True if the argument takes a collection; false otherwise.
    */
   public void setArgCollection(int nOrdinal, boolean bCollection)
   {
      change();
      m_argCollectionArray[nOrdinal] = bCollection;
   }

   /**
    * Gets whether the argument takes a collection.
    * @param nOrdinal The zero-based ordinal of the argument.
    * @return True if the argument takes a collection; false otherwise.
    */
   public boolean getArgCollection(int nOrdinal)
   {
      derive();

      return m_argCollectionArray[nOrdinal];
   }

   /**
    * Sets the derived flag.
    * @param bDerived True to derive type mappings from equivalent method in the base class.
    */
   public void setDerived(boolean bDerived)
   {
      change();
      m_bDerived = bDerived;
   }

   /**
    * Gets the derived flag.
    * @return True to derive type mappings from equivalent method in the base class.
    */
   public boolean isDerived()
   {
      return m_bDerived;
   }

   /**
    * Inherit type from the equivalent method in the base class.
    */
   protected void derive()
   {
      if (m_bDerived)
      {
         assert !m_holder.isForward();

         MethodObject baseMethod = null;

         for (int nBase = 0, nCount = m_holder.getBaseCount(); nBase < nCount && baseMethod == null; nBase++)
         {
            ClassObject base = m_holder.getBase(nBase);

            baseMethod = base.findMethod(getSymbol(), m_nArgCount);
         }

         if (baseMethod instanceof TypedMethodObject)
         {
            TypedMethodObject typed = (TypedMethodObject)baseMethod;

            m_type = typed.getResultType();
            m_bCollection = typed.isCollection();

            for (int i = 0; i < m_nArgCount; i++)
            {
               m_argTypeArray[i] = typed.getArgType(i);
               m_argCollectionArray[i] = typed.getArgCollection(i);
            }
         }

         m_bDerived = false;
      }
   }

   /**
    * Exposes members from TypedMethodObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      classObject.addMethod(":type", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object typeSpec = machine.getArg(1, nArgCount);

            if (typeSpec instanceof Symbol)
            {
               method.setResultType(TypedMetaclassObject.getForwardType((Symbol)typeSpec, method, machine));
            }
            else
            {
               throw new IllegalArgumentException();
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":type", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);

            machine.returnValue(method.getResultType(), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":arg-types", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object typeSpec = machine.getArg(1, nArgCount);

            if (typeSpec instanceof Pair)
            {
               Pair types = (Pair)typeSpec;
               int nArg = 0;

               while (types != null)
               {
                  Symbol typeSym = (Symbol)types.getHead();

                  method.setArgType(nArg, TypedMetaclassObject.getForwardType(typeSym, method, machine));
                  nArg++;
                  types = types.getNext();
               }
            }
            else
            {
               throw new IllegalArgumentException();
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":arg-types", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            int nCount = method.getArgCount();
            Pair types = null;

            for (int i = nCount - 1; i >= 0; i--)
            {
               types = new Pair(method.getArgType(i), types);
            }

            machine.returnValue(types, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":collection", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object typeSpec = (Object)machine.getArg(1, nArgCount);

            if (typeSpec instanceof Boolean)
            {
               method.setCollection(((Boolean)typeSpec).booleanValue());
            }
            else
            {
               throw new IllegalArgumentException();
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":collection?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);

            machine.returnValue(Boolean.valueOf(method.isCollection()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":arg-collections", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object typeSpec = (Object)machine.getArg(1, nArgCount);

            if (typeSpec instanceof Pair)
            {
               Pair types = (Pair)typeSpec;
               int nArg = 0;

               while (types != null)
               {
                  boolean bCollection = Intrinsic.isTrue(types.getHead());

                  method.setArgCollection(nArg, bCollection);
                  nArg++;
                  types = types.getNext();
               }
            }
            else
            {
               throw new IllegalArgumentException();
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":arg-collections", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            int nCount = method.getArgCount();
            Pair types = null;

            for (int i = nCount - 1; i >= 0; i--)
            {
               types = new Pair(Boolean.valueOf(method.getArgCollection(i)), types);
            }

            machine.returnValue(types, nArgCount);

            return false;
         }
      });

      classObject.addMethod(":derived", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);

            method.setDerived(Intrinsic.isTrue(machine.getArg(1, nArgCount)));
            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(VALIDATE_ARGS, 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object errorLocation = machine.getArg(1, nArgCount);
            Pair args = (Pair)machine.getArg(2, nArgCount);
            int nArgListLength = Pair.length(args);

            if (nArgListLength < method.getMinArgCount() || (!method.isVarArg() && nArgListLength > method.getArgCount()))
            {
               throw new InvocationException("err.scripting.methodLookup",
                  new Object[]{method.getName(), Primitive.createInteger(nArgListLength), method.getHolder().getName()});
            }

            Function fun = machine.getGlobalEnvironment().findFunction(method.getHolder(), method.getSymbol(), method.getMinArgCount());

            if (fun == null)
            {
               fun = method.getFunction();
               machine.getGlobalEnvironment().addFunction(method.getHolder(), method.getSymbol(), method.getMinArgCount(), fun);
            }

            for (int nArg = 0; args != null; nArg++)
            {
               Object arg = args.getHead();

               if (nArg < method.getArgCount() - 1 || !method.isVarArg())
               {
                  TypedMetaclassObject.validateValue(arg,
                     method.getArgType(nArg), method.getArgCollection(nArg), errorLocation, method.ARGUMENT_HANDLER);
               }
               else
               {
                  // Check type of the variable argument
                  Pair varArg = (Pair)arg;

                  while (varArg != null)
                  {
                     TypedMetaclassObject.validateValue(varArg.getHead(),
                        method.getArgType(nArg), method.getArgCollection(nArg), errorLocation, method.ARGUMENT_HANDLER);
                     varArg = varArg.getNext();
                  }
               }

               args = args.getNext();
            }

            machine.returnValue(null, nArgCount);

            return false;
         }
      });

      classObject.addMethod(VALIDATE_RESULT, 2, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            TypedMethodObject method = (TypedMethodObject)machine.getArg(0, nArgCount);
            Object errorLocation = machine.getArg(1, nArgCount);
            Object retValue = (Object)machine.getArg(2, nArgCount);

            TypedMetaclassObject.validateValue(retValue, method.getResultType(), method.isCollection(), errorLocation, method.RESULT_HANDLER);
            machine.returnValue(retValue, nArgCount);

            return false;
         }
      });
   }
}
