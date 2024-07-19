// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration;

import java.util.Locale;

import nexj.core.meta.Component;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Channel;
import nexj.core.rpc.ServerException;
import nexj.core.runtime.Context;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Receiver managing an invocation context.
 */
public abstract class ContextReceiver extends Receiver implements Initializable
{
   // associations

   /**
    * The invocation context component.
    */
   protected Component m_contextComponent;

   // operations

   /**
    * Sets the invocation context component.
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * @return The invocation context component.
    */
   public Component getContextComponent()
   {
      return m_contextComponent;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_contextComponent == null)
      {
         m_contextComponent = Repository.getMetadata().getComponent("System.InvocationContext");
      }
   }

   /**
    * @see nexj.core.integration.Receiver#startStat(nexj.core.meta.integration.Channel, nexj.core.runtime.InvocationContext)
    */
   protected long startStat(Channel channel, InvocationContext context)
   {
      return 0;
   }

   /**
    * @see nexj.core.integration.Receiver#endStat(long, nexj.core.meta.integration.Channel, nexj.core.runtime.InvocationContext)
    */
   protected void endStat(long lStartTime, Channel channel, InvocationContext context)
   {
   }

   /**
    * Runs a receiver strategy with an invocation context.
    * @param strategy The object to run.
    * @param channel The integration channel.
    * @param sType The integration message type name.
    */
   protected void run(ContextRunnable strategy, Channel channel, String sType)
   {
      Context contextSaved = ThreadContextHolder.getContext();
      Logger logger = channel.getLogger();
      InvocationContext context = null;
      long lStartTime = 0;
      int nCookie = -1;

      try
      {
         if (!strategy.isEnabled())
         {
            return;
         }

         context = (InvocationContext)m_contextComponent.getInstance(null);

         String sAddress = strategy.getClientAddress();

         context.setClientAddress(sAddress);

         if (sAddress != null)
         {
            nCookie = Logger.pushContext(sAddress);
         }

         String sUser = strategy.getUser();

         context.initialize((sUser != null) ? new SimplePrincipal(sUser) : null);
         lStartTime = super.startStat(channel, context);

         int nUserCookie = Logger.pushContext(context.getPrincipal().getName());

         if (nCookie == -1)
         {
            nCookie = nUserCookie;
         }

         if (logger.isDebugEnabled())
         {
            logger.debug("Received a " + sType + " message from " +
               context.getPrincipal().getName() + ((sAddress == null) ? "" : " @ " + sAddress) +
               " on channel \"" + channel.getName() + "\"");
         }

         strategy.run(context);
         context.complete(true);

         if (logger.isDebugEnabled())
         {
            logger.debug("Completed processing the " + sType + " message");
         }
      }
      catch (Throwable e)
      {
         int nLevel = (ObjUtil.isError(e)) ? Logger.ERROR : Logger.DEBUG;

         if (logger.isLevelEnabled(nLevel))
         {
            logger.log(nLevel, "Error processing the " + sType + " message", e);
         }

         try
         {
            if (context != null)
            {
               context.complete(false);
            }
         }
         catch (Throwable t)
         {
            logger.error("Unable to complete the processing", t);
         }

         try
         {
            strategy.err(e, context);
         }
         catch (Throwable t)
         {
            logger.error("Unable to handle the error", t);
         }

         throw new ServerException("err.rpc." + sType.toLowerCase(Locale.ENGLISH), e);
      }
      finally
      {
         if (nCookie != -1)
         {
            Logger.resetContext(nCookie);
         }

         super.endStat(lStartTime, channel, context);
         ThreadContextHolder.setContext(contextSaved);
      }
   }

   // inner classes

   /**
    * Strategy for processing with an invocation context.
    */
   public interface ContextRunnable
   {
      /**
       * @return True to run the strategy.
       */
      boolean isEnabled() throws Throwable;

      /**
       * @return The client address, or null if unknown.
       */
      String getClientAddress() throws Throwable;

      /**
       * @return The invocation context user.
       */
      String getUser() throws Throwable;

      /**
       * Runs logic with an invocation context.
       * @param context The invocation context. 
       */
      void run(InvocationContext context) throws Throwable;

      /**
       * Handles an exception.
       * @param t The exception to handle.
       * @param context The invocation context. 
       */
      void err(Throwable t, InvocationContext context) throws Throwable;
   }
}
