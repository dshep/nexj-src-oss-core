package nexj.core.runtime;

import nexj.core.scripting.Function;
import nexj.core.scripting.GlobalEnvironment;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * A Java-based client for performing NexJ SOA service invocations.
 */
public class SOAClient
{
   // constants

   /**
    * The symbol soa:create-proxy to create a new proxy to a service.
    */
   public final static Symbol SOA_CREATE_PROXY = Symbol.define("soa:create-proxy");

   // associations

   /**
    * The VM for invoking the proxy.
    */
   Machine m_machine;

   /**
    * The service proxy.
    */
   Function m_proxy;

   // constructors

   /**
    * Creates a new client to the given service.
    * @param service The fully-qualified service name.
    * @param properties The proxy properties, if any.
    * @param context The context for invoking the service.
    */
   public SOAClient(Symbol service, Pair properties, Context context)
   {
      m_machine = context.getMachine();

      GlobalEnvironment env = m_machine.getGlobalEnvironment();
      Function createProxy = (Function)env.getVariable(SOA_CREATE_PROXY);
      Object serviceClass = env.getVariable(service);

      m_proxy = (Function)m_machine.invoke(createProxy, serviceClass, properties);
   }

   /**
    * Creates a new client to the given service.
    * @param service The fully-qualified service name.
    * @param context The context for invoking the service.
    */
   public SOAClient(Symbol service, Context context)
   {
      this(service, null, context);
   }

   /**
    * Creates a new client to the given service.
    * @param sService The fully-qualified service name.
    * @param context The context for invoking the service.
    */
   public SOAClient(String sService, Context context)
   {
      this(Symbol.define(sService), context);
   }

   /**
    * Creates a new client to the given service.
    * @param sService The fully-qualified service name.
    * @param properties The proxy properties, if any.
    * @param context The context for invoking the service.
    */
   public SOAClient(String sService, Pair properties, Context context)
   {
      this(Symbol.define(sService), properties, context);
   }

   /**
    * Creates a new client to the given service. Service invocations are performed using the thread context
    * at the time of SOAClient creation.
    * @param service The fully-qualified service name.
    */
   public SOAClient(Symbol service)
   {
      this(service, ThreadContextHolder.getContext());
   }

   /**
    * Creates a new client to the given service. Service invocations are performed using the thread context
    * at the time of SOAClient creation.
    * @param sService The fully-qualified service name.
    */
   public SOAClient(String sService)
   {
      this(sService, ThreadContextHolder.getContext());
   }

   // operations

   /**
    * Invokes a zero-argument method on the service.
    * @param method The method to invoke.
    * @return The method return value.
    */
   public Object invoke(Symbol method)
   {
      return m_machine.invoke(m_proxy, method, (Object[])null);
   }

   /**
    * Invokes a zero-argument method on the service.
    * @param sMethod The method to invoke.
    * @return The method return value.
    */
   public Object invoke(String sMethod)
   {
      return invoke(Symbol.define(sMethod));
   }

   /**
    * Invokes a method on the service.
    * @param method The method to invoke.
    * @param argArray The method arguments; null for no arguments.
    * @return The method return value.
    */
   public Object invoke(Symbol method, Object[] argArray)
   {
      return m_machine.invoke(m_proxy, method, argArray);
   }

   /**
    * Invokes a method on the service.
    * @param sMethod The method to invoke.
    * @param argArray The method arguments; null for no arguments.
    * @return The method return value.
    */
   public Object invoke(String sMethod, Object[] argArray)
   {
      return invoke(Symbol.define(sMethod), argArray);
   }

   /**
    * Invokes a method on the service.
    * @param method The method to invoke.
    * @param arg1 The first argument to the method.
    * @param argArray The remaining method arguments; null for no additional arguments.
    * @return The method return value.
    */
   public Object invoke(Symbol method, Object arg1, Object[] argArray)
   {
      return m_machine.invoke(m_proxy, method, arg1, argArray);
   }

   /**
    * Invokes a method on the service.
    * @param sMethod The method to invoke.
    * @param arg1 The first argument to the method.
    * @param argArray The remaining method arguments; null for no additional arguments.
    * @return The method return value.
    */
   public Object invoke(String sMethod, Object arg1, Object[] argArray)
   {
      return invoke(Symbol.define(sMethod), arg1, argArray);
   }

   /**
    * Invokes a single-argument method on the service.
    * @param method The method to invoke.
    * @param arg1 The argument to the method.
    * @return The method return value.
    */
   public Object invoke(Symbol method, Object arg1)
   {
      return invoke(method, arg1, (Object[])null);
   }

   /**
    * Invokes a single-argument method on the service.
    * @param sMethod The method to invoke.
    * @param arg1 The argument to the method.
    * @return The method return value.
    */
   public Object invoke(String sMethod, Object arg1)
   {
      return invoke(Symbol.define(sMethod), arg1, (Object[])null);
   }

   /**
    * Invokes a method on the service. Returns multiple return values in the result argument.
    * @param method The method to invoke.
    * @param arg1 The first argument to the method.
    * @param argArray The remaining method arguments; null for no additional arguments.
    * @return The method's return values.
    */
   public Object[] invokeMultiValue(Symbol method, Object arg1, Object[] argArray)
   {
      Object result = m_machine.invokeValuesConsumer(m_proxy, method, arg1, argArray, new Function() {
         public boolean invoke(int nArgCount, Machine machine)
         {
            Object[] resultArray = new Object[nArgCount];

            machine.getArgs(resultArray);
            machine.returnValue(resultArray, nArgCount);

            return false;
         }
      });

      return (Object[])result;
   }

   /**
    * Invokes a method on the service. Returns multiple return values in the result argument.
    * @param sMethod The method to invoke.
    * @param arg1 The first argument to the method.
    * @return The method's return values.
    */
   public Object[] invokeMultiValue(String sMethod, Object arg1)
   {
      return invokeMultiValue(Symbol.define(sMethod), arg1, (Object[])null);
   }
}
