// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.io.IOException;
import java.sql.Timestamp;

import nexj.core.meta.Accessor;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;

/**
 * The system timer implementation.
 */
public class SysTimer implements InvocationContextAware
{
   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * Schedules a delayed event invocation.
    * @param accessor The metaclass or instance to invoke.
    * @param event The event to invoke.
    * @param tmStart The start time. Null to use the current time.
    * @param period The period in ms. Null or 0 for an aperiodic invocation.
    * @param sUser The account under which to execute the timer, null to use the default.
    * @return The timer object.
    */
   public Instance invoke(Metaclass metaclass, Accessor accessor, Symbol event,
      Timestamp tmStart, Number period, String sUser, ActionContext actx)
   {
      Instance instance = new Instance(metaclass, m_context);
      
      if (tmStart == null)
      {
         tmStart = new Timestamp(System.currentTimeMillis());
      }
      
      if (period == null || period.longValue() < 0)
      {
         period = Primitive.ZERO_LONG;
      }

      instance.setNew();
      instance.setValue("start", tmStart);
      instance.setValue("period", period);
      instance.setValue("next", tmStart);
      instance.setValue("principal", sUser);
      
      if (accessor instanceof Metaclass)
      {
         TransferObject tobj = new TransferObject(((Metaclass)accessor).getName(), 0);
         Request request = new Request();

         tobj.setEventName(event.getName());
         request.addInvocation(tobj);

         try
         {
            instance.setValue("data", Binary.fromObject(request));
         }
         catch (IOException e)
         {
            throw new RPCException("err.rpc.requestSerialization", e);
         }
      }
      else
      {
         instance.setValue("object", accessor);
         instance.setValue("event", event);
      }

      instance.invoke("create");
      
      return instance;
   }
}
