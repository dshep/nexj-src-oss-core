// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.sys;

import java.io.Serializable;
import java.util.Collection;

import nexj.core.meta.Accessor;
import nexj.core.meta.Metaclass;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.cluster.NodeManager;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;

/**
 * Message queue wrapper.
 */
public class SysQueue implements InvocationContextAware
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
    * Asynchronously invokes an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke. 
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor accessor, Symbol event, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, null, null, null, -1, -1, false, true);
   }

   /**
    * Asynchronously invokes an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor accessor, Symbol event, Object args, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, toArray(args), null, null, -1, -1, false, true);
   }

   /**
    * Asynchronously invokes an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param sChannel The destination channel name.
    */
   public void invokeLocal(Metaclass metaclass, Accessor accessor, Symbol event, Object args,
      int nPriority, long lTTL, String sChannel, ActionContext actx)
   {
      TransferObject properties = new TransferObject(1);

      properties.setValue("receiver", ((NodeManager)m_context.getComponentInstance("System.ClusterManager")).getHTTPNode());
      m_context.getUnitOfWork().addMessage(sChannel, accessor, event, toArray(args),
         null, null, nPriority, lTTL, false, properties);
   }

   /**
    * Asynchronously invokes an event.
    * @param instance The instance to invoke.
    * @param event The symbol of the event to invoke.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor instance, Symbol event, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(instance, event, null, null, null, nPriority, lTTL, false, false);
   }

   /**
    * Asynchronously invokes an event.
    * @param instance The instance to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor instance, Symbol event, Object args, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(instance, event, toArray(args), null, null, nPriority, lTTL, false, false);
   }

   /**
    * Asynchronously invokes an event with a correlation instance.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param correlator The correlation object, which will be invoked after the request has been processed.
    * @param corEvent The correlation event.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor accessor, Symbol event,
      Accessor correlator, Symbol corEvent, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, null, correlator, corEvent, nPriority, lTTL, false, false);
   }

   /**
    * Asynchronously invokes an event with a correlation instance, using a given queue.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param correlator The correlation object, which will be invoked after the request has been processed.
    * @param corEvent The correlation event.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param sChannel The destination channel name.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor accessor, Symbol event,
      Accessor correlator, Symbol corEvent, int nPriority, long lTTL, String sChannel, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(sChannel, accessor, event, null, correlator, corEvent, nPriority, lTTL, false);
   }

   /**
    * Asynchronously invokes an event with a correlation instance, using a given queue.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @param correlator The correlation object, which will be invoked after the request has been processed.
    * @param corEvent The correlation event.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @param sChannel The destination channel name.
    * @return The cached object, or null if not found.
    */
   public void invoke(Metaclass metaclass, Accessor accessor, Symbol event, Object args,
      Accessor correlator, Symbol corEvent, int nPriority, long lTTL, String sChannel, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(sChannel, accessor, event, toArray(args), correlator, corEvent, nPriority, lTTL, false);
   }

   /**
    * Asynchronously broadcasts an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke. 
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      boolean bNoLocal, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, null, null, null, -1, -1, true, bNoLocal);
   }

   /**
    * Asynchronously broadcasts an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke. 
    * @param args The event arguments. Can be null.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      Object args, boolean bNoLocal, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, toArray(args), null, null, -1, -1, true, bNoLocal);
   }

   /**
    * Asynchronously broadcasts an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      boolean bNoLocal, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, null, null, null, nPriority, lTTL, true, bNoLocal);
   }

   /**
    * Asynchronously broadcasts an event.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param lTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      Object args, boolean bNoLocal, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, toArray(args), null, null, nPriority, lTTL, true, bNoLocal);
   }

   /**
    * Asynchronously invokes an event with a correlation instance.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param correlator The correlation object, which will be invoked after the request has been processed.
    * @param corEvent The correlation event.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param nTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      Accessor correlator, Symbol corEvent, boolean bNoLocal,
      int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, null, correlator, corEvent, nPriority, lTTL, true, bNoLocal);
   }

   /**
    * Asynchronously invokes an event with a correlation instance.
    * @param accessor The instance or metaclass to invoke.
    * @param event The symbol of the event to invoke.
    * @param args The event arguments. Can be null.
    * @param correlator The correlation object, which will be invoked after the request has been processed.
    * @param corEvent The correlation event.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param nTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void broadcast(Metaclass metaclass, Accessor accessor, Symbol event,
      Object args, Accessor correlator, Symbol corEvent, boolean bNoLocal,
      int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(accessor, event, toArray(args), correlator, corEvent, nPriority, lTTL, true, bNoLocal);
   }

   /**
    * Asynchronously sends a message.
    * @param sChannel The message destination channel name.
    * @param message The object to send.
    * @param properties The message properties.
    * @return The cached object, or null if not found.
    */
   public void send(Metaclass metaclass, String sChannel, Serializable message, TransferObject properties, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(sChannel, message, properties, -1, -1, false);
   }

   /**
    * Asynchronously sends a message.
    * @param sChannel The message destination channel name.
    * @param message The object to send.
    * @param properties The message properties.
    * @param bNoLocal True to exclude the local node from the broadcast.
    * @param nPriority The priority of the message, 0..9, or -1 to use the default value.
    * @param nTTL The message time to live in ms, or -1 to use the default value.
    * @return The cached object, or null if not found.
    */
   public void send(Metaclass metaclass, String sChannel, Serializable message, TransferObject properties,
      boolean bNoLocal, int nPriority, long lTTL, ActionContext actx)
   {
      m_context.getUnitOfWork().addMessage(sChannel, message, properties, nPriority, lTTL, bNoLocal);
   }

   /**
    * Converts an object to an array, if possible.
    * @param obj The object to convert.
    * @return The converted object.
    */
   protected static Object[] toArray(Object obj)
   {
      if (obj instanceof Pair)
      {
         return Pair.toArray((Pair)obj);
      }

      if (obj instanceof Collection)
      {
         return ((Collection)obj).toArray();
      }

      return (Object[])obj;
   }
}
