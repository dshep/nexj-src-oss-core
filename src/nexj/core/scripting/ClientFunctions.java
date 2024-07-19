// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting;

import java.lang.reflect.Constructor;

import nexj.core.meta.Primitive;
import nexj.core.meta.Type;
import nexj.core.meta.TypeMismatchException;
import nexj.core.meta.Typed;
import nexj.core.meta.ui.ClassMetaMap;
import nexj.core.scripting.Intrinsic.IsolatedFunctions;
import nexj.core.scripting.Intrinsic.JavaIntrinsicFunction;
import nexj.core.scripting.object.ClassObject;
import nexj.core.scripting.object.ObjectOriented;
import nexj.core.util.ObjUtil;
import nexj.core.util.ole.OLEUtil;

/**
 * Functions that require client framework; they are not present in minimal
 * runtime.
 */
public class ClientFunctions extends IsolatedFunctions
{
   /**
    * Constructor for OLEObject.
    */
   private static Constructor s_oleObjectConstructor;
   
   static
   {
      try
      {
         s_oleObjectConstructor = Class.forName("nexj.core.scripting.OLEObject")
            .getConstructor(new Class[] {String.class, String.class});
      }
      catch (Throwable t)
      {
      }
   }
   
   /**
    * Extends the basic instance? implementation.
    * 
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getInstancePFunction()
    */
   public IntrinsicFunction getInstancePFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 2);

            Object obj = machine.getArg(0, nArgCount);
            Object clazz = machine.getArg(1, nArgCount);
            boolean bResult;

            if (clazz instanceof Type)
            {
               if (obj instanceof Typed)
               {
                  bResult = ((Type)clazz).isUpcast(((Typed)obj).getType());
               }
               else if (obj != null)
               {
                  Class objClass = obj.getClass();
                  Primitive objType = Primitive.primitiveOf(objClass);

                  bResult = ((Type)clazz).isUpcast(objType) || clazz == Primitive.ANY;
               }
               else
               {
                  bResult = false;
               }
            }
            else if (clazz instanceof ClassObject)
            {
               bResult = (obj instanceof ObjectOriented && ((ObjectOriented)obj).getClassObject().isA((ClassObject)clazz));
            }
            else if (clazz instanceof Class)
            {
               bResult = (obj != null) && ((Class)clazz).isAssignableFrom(obj.getClass());
            }
            else if (clazz instanceof Symbol)
            {
               Type type = ((ClassMetaMap)machine.getContext().getContextMetadata()).getClassMeta(((Symbol)clazz).getName());

               bResult = (type != null && obj instanceof Typed && type.isUpcast(((Typed)obj).getType()));
            }
            else
            {
               throw new TypeMismatchException(getSymbol());
            }

            machine.returnValue(Boolean.valueOf(bResult), nArgCount);

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.INSTANCE_P;
         }
      };
   }

   /**
    * @see nexj.core.scripting.Intrinsic.IsolatedFunctions#getObjectFunction()
    */
   public IntrinsicFunction getObjectFunction()
   {
      return new JavaIntrinsicFunction()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            verifyArgCount(nArgCount, 1, 2);

            Object progId = machine.getArg(0, nArgCount);

            if (!(progId instanceof String))
            {
               throw new TypeMismatchException(getSymbol());
            }

            Object location = null;

            if (nArgCount > 1)
            {
               location = machine.getArg(1, nArgCount);

               if (location != null && !(location instanceof String))
               {
                  throw new TypeMismatchException(getSymbol());
               }
            }

            if (!OLEUtil.isAvailable() || s_oleObjectConstructor == null)
            {
               throw new ScriptingException("err.ole.unavailable");
            }

            try
            {
               machine.returnValue(
                  s_oleObjectConstructor.newInstance(new Object[]{(String)progId, (String)location}),
                  nArgCount);
            }
            catch (Throwable t)
            {
               ObjUtil.rethrow(t);
            }

            return false;
         }

         public Symbol getSymbol()
         {
            return Symbol.OBJECT;
         }
      };
   }
}
