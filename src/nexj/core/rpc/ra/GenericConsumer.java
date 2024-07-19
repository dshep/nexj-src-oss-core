// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;

import javax.resource.spi.endpoint.MessageEndpoint;
import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkEvent;
import javax.resource.spi.work.WorkListener;

import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Generic message endpoint consumer, corresponding to one active instance of an MDB.
 */
public abstract class GenericConsumer implements Work, WorkListener
{
   // attributes

   /**
    * True if the message processing has failed.
    */
   protected boolean m_bFailed;

   /**
    * Dispose of the consumer after processing the message.
    */
   protected boolean m_bDispose;

   // associations
   
   /**
    * The consumer pool.
    */
   protected GenericConsumerPool m_pool;

   /**
    * The message endpoint.
    */
   protected MessageEndpoint m_endpoint;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Constructs the consumer.
    * @param pool The consumer pool.
    * @param logger The class logger.
    */
   protected GenericConsumer(GenericConsumerPool pool, Logger logger) throws Throwable
   {
      m_pool = pool;
      m_logger = logger;

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Initializing " + this);
      }

      try
      {
         m_endpoint = init();
      }
      finally
      {
         if (m_endpoint == null)
         {
            dispose();
         }
      }
   }

   // operations

   /**
    * Initializes the consumer.
    * @return The message endpoint.
    */
   protected abstract MessageEndpoint init() throws Throwable;

   /**
    * Schedules the consumer to run on a separate thread.
    */
   protected void activate() throws Throwable
   {
      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Activating " + this);
      }

      try
      {
         m_pool.getAdapter().getContext().getWorkManager().scheduleWork(this, 10000, null, this);
      }
      catch (Throwable e)
      {
         if (m_logger.isWarnEnabled())
         {
            m_logger.warn("Unable to run " + this, e);
         }

         throw e;
      }
   }

   /**
    * Consumes all the messages from the session.
    * Invokes, directly or indirectly, consume(Object).
    * @see GenericConsumer#consume(Object)
    * @return True if an XA transaction has been started.
    */
   protected abstract boolean consume() throws Throwable;

   /**
    * Closes the consumer session.
    */
   protected abstract void close();

   /**
    * @see java.lang.Runnable#run()
    */
   public void run()
   {
      m_bFailed = false;
      m_bDispose = false;
      boolean bXA = false;
      Throwable t = null;

      try
      {
         m_endpoint.beforeDelivery(getMethod());

         try
         {
            bXA = consume();
         }
         finally
         {
            try
            {
               m_endpoint.afterDelivery();
            }
            catch (Throwable e)
            {
               t = e;
               m_bFailed = true;
               m_bDispose = true;
            }
         }
      }
      catch (Throwable e)
      {
         t = e;
         m_bFailed = true;
      }

      if (t != null && m_logger.isDebugEnabled())
      {
         m_logger.debug("Consumer failure in " + this, t);
      }

      finish(bXA);

      if (m_bDispose)
      {
         dispose();
      }
   }

   /**
    * Template method invoked from run() to finish the session processing.
    * @param bXA True if an XA transaction has been started.
    */
   protected void finish(boolean bXA)
   {
   }

   /**
    * @see javax.resource.spi.work.Work#release()
    */
   public void release()
   {
   }

   /**
    * @see javax.resource.spi.work.WorkListener#workAccepted(javax.resource.spi.work.WorkEvent)
    */
   public void workAccepted(WorkEvent evt)
   {
   }

   /**
    * @see javax.resource.spi.work.WorkListener#workStarted(javax.resource.spi.work.WorkEvent)
    */
   public void workStarted(WorkEvent evt)
   {
   }

   /**
    * @see javax.resource.spi.work.WorkListener#workCompleted(javax.resource.spi.work.WorkEvent)
    */
   public void workCompleted(WorkEvent evt)
   {
      deactivate();
   }

   /**
    * @see javax.resource.spi.work.WorkListener#workRejected(javax.resource.spi.work.WorkEvent)
    */
   public void workRejected(WorkEvent evt)
   {
      deactivate();
   }

   /**
    * Deactivates the consumer, releasing it back to the pool.
    */
   protected void deactivate()
   {
      if (m_endpoint != null)
      {
         m_pool.deactivate(this);

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Deactivated " + this);
         }
      }
   }

   /**
    * Releases all the resources associated with the consumer.
    */
   public void dispose()
   {
      if (m_endpoint != null)
      {
         try
         {
            m_endpoint.release();
         }
         catch (Throwable e)
         {
         }

         m_endpoint = null;
      }

      close();

      m_pool.remove(this);

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Disposed of " + this);
      }
   }

   /**
    * @return The MDB delivery method.
    */
   protected abstract Method getMethod();

   /**
    * Delivers the message to the MDB.
    * @param message The message to deliver.
    */
   protected abstract void deliver(Object message) throws Throwable;

   /**
    * Consumes a message.
    * @param message The message to consume.
    */
   protected void consume(Object message)
   {
      try
      {
         deliver(message);
      }
      catch (Throwable e)
      {
         m_bFailed = true;

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Message processing error in " + this, e);
         }
      }
   }

   /**
    * Gets a class method using a privileged action.
    * @param clazz The class object.
    * @param sName The method name.
    * @param args The method arguments.
    * @return The method object. 
    */
   protected static Method getMethod(final Class clazz, final String sName, final Class[] args)
   {
      return (Method)AccessController.doPrivileged(
         new PrivilegedAction()
         {
            public Object run()
            {
               try
               {
                  return clazz.getMethod(sName, args);
               }
               catch (Throwable t)
               {
                  throw new RuntimeException("Method " + clazz.getName() + '.' + sName + " () not found", t);
               }
            }
         });
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this)); 
      buf.append('@');
      buf.append(System.identityHashCode(this));
      buf.append('(');
      buf.append(m_pool);
      buf.append(')');

      return buf.toString();
   }
}
