// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.meta.Primitive;
import nexj.core.scripting.Compiler;
import nexj.core.scripting.Function;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Lookup;
import nexj.core.util.TextPosition;

public class TypedGrandMetaclassObject extends GrandMetaclassObject
{
   // constants

   /**
    * The name of the typed grand metaclass.
    */
   public final static Symbol SYMBOL = Symbol.define("sys:TypedGrandMetaclass");

   /**
    * The :add-method symbol.
    */
   protected final static Symbol ADD_METHOD = Symbol.define(":add-method");

   /**
    * The :find-method symbol.
    */
   protected final static Symbol FIND_METHOD = Symbol.define(":find-method");

   /**
    * The #method symbol.
    */
   protected final static Symbol METHOD = Symbol.define("#method");

   /**
    * The typed grand metaclass singleton.
    */
   public final static TypedGrandMetaclassObject INSTANCE = new TypedGrandMetaclassObject();

   // constructors

   /**
    * Constructs the metaclass object.
    */
   protected TypedGrandMetaclassObject()
   {
      super(INSTANCE, SYMBOL);

      m_class = null;
      addMethodValidation(this);
      addBase(GrandMetaclassObject.INSTANCE);  // provides base implementation of :add-method
      m_class = this;
   }

   // operations

   /**
    * Changes :add-method to add argument validation logic to the method being added.
    * @param metaclass The metaclass with :add-method to redefine.
    */
   public static void addMethodValidation(final MetaclassObject metaclass)
   {
      metaclass.addMethod(metaclass.getMetaclass().createMethod(metaclass, ADD_METHOD, 4, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            ClassObject classObject = (ClassObject)machine.getArg(0, nArgCount);
            Symbol nameSym = (Symbol)machine.getArg(1, nArgCount);
            Object body = machine.getArg(3, nArgCount);
            Pair args;
            Object methodArgExpr;
            Symbol thisSym;

            if (body instanceof Pair)
            {
               Pair code = (Pair)body;

               if (Symbol.LAMBDA.equals(code.getHead()))
               {
                  if (code.getNext() == null)
                  {
                     throw new DeclarationException("err.compiler.minArgCount",
                        new Object[]{Symbol.LAMBDA, Primitive.createInteger(2)});
                  }

                  if (!(code.getNext().getHead() instanceof Pair))
                  {
                     throw new DeclarationException("err.compiler.callArgs");
                  }

                  args = (Pair)code.getNext().getHead();
                  methodArgExpr = args.getTail();

                  if (!(args.getHead() instanceof Symbol))
                  {
                     throw new DeclarationException("err.compiler.lambdaArg");
                  }

                  thisSym = (Symbol)args.getHead();
               }
               else
               {
                  throw new DeclarationException("err.scripting.methodBody");
               }
            }
            else
            {
               throw new DeclarationException("err.meta.sexprSyntax", new Object[]{body});
            }

            int nMinimumMethodArgCount = 0;
            boolean bVarArg = false;
            Object arg = methodArgExpr;

            while (arg instanceof Pair)
            {
               nMinimumMethodArgCount++;
               arg = ((Pair)arg).getTail();
            }

            if (arg instanceof Symbol)
            {
               bVarArg = true;
            }
            else if (arg != null)
            {
               throw new DeclarationException("err.compiler.lambdaArg");
            }

            Pair execExpr;

            if (!bVarArg)
            {
               execExpr = new Pair(body, args);
            }
            else
            {
               Pair tail = null;

               // Change methodArgExpr to be a proper list
               if (methodArgExpr instanceof Pair)
               {
                  Pair pair = (Pair)methodArgExpr;

                  methodArgExpr = tail = new Pair(pair.getHead());
                  arg = pair.getTail();

                  while (arg instanceof Pair)
                  {
                     pair = (Pair)arg;
                     arg = pair.getTail();
                     tail.setTail(new Pair(pair.getHead()));
                     tail = tail.getNext();
                  }

                  tail.setTail(new Pair((Symbol)arg));
               }
               else
               {
                  methodArgExpr = new Pair(methodArgExpr);
               }

               execExpr = new Pair(Symbol.APPLY, new Pair(body, new Pair(thisSym, methodArgExpr)));
            }

            // Interpose the method invocation with calls to verify its argument and its result
            Pair code = Pair.list(
               Symbol.LAMBDA,
               args,
               Pair.list(Symbol.DEFINE, METHOD,
                  Pair.list(classObject, Pair.quote(FIND_METHOD), Pair.quote(nameSym), Primitive.createInteger(nMinimumMethodArgCount))
               ),
               Pair.list(METHOD, Pair.quote(TypedMethodObject.VALIDATE_ARGS), thisSym, new Pair(Symbol.LIST, methodArgExpr)),
               Pair.list(METHOD, Pair.quote(TypedMethodObject.VALIDATE_RESULT), thisSym, execExpr)
            );

            machine.setArg(3, nArgCount, code);

            GlobalEnvironment env = machine.getGlobalEnvironment();
            Lookup posMap = env.getTextPositionMap();

            if (posMap != null)
            {
               String sURL = Compiler.findPosURL(body, posMap);

               if (sURL != null)
               {
                  posMap.put(code, new TextPosition(0, 0, sURL));
               }
            }

            return metaclass.resolveBaseFunction(ADD_METHOD, 4).invoke(nArgCount, machine);
         }
      }), true);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createAttribute(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol)
    */
   protected AttributeObject createAttribute(ClassObject classObject, Symbol symbol)
   {
      return new TypedAttributeObject(classObject, symbol);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#createMethod(nexj.core.scripting.object.ClassObject, nexj.core.scripting.Symbol, int, boolean, nexj.core.scripting.Function)
    */
   protected MethodObject createMethod(ClassObject classObject, Symbol symbol,
      int nArgCount, boolean bVarArg, Function function)
   {
      return new TypedMethodObject(classObject, symbol, nArgCount, bVarArg, function);
   }

   /**
    * @see nexj.core.scripting.object.MetaclassObject#getObjectClassSymbol()
    */
   protected Symbol getObjectClassSymbol()
   {
      return TypedMetaclassObject.TYPED_METACLASS_SYMBOL;
   }
}
