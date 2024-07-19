// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.util.ArrayList;
import java.util.List;

import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.resource.spi.work.Work;

import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Generic consumer endpoint pool, corresponding to the MDBs with the same configuration object.
 */
public abstract class GenericConsumerPool implements Work
{
   // attributes

   /**
    * True to connect.
    */
   protected boolean m_bConnect;

   /**
    * True to shut down the pool.
    */
   protected boolean m_bShutdown;

   /**
    * True if the pool is running.
    */
   protected boolean m_bRunning;

   /**
    * True if the maximum pool size has been reached.
    */
   protected boolean m_bMaxReached;

   /**
    * The total consumer count.
    */
   protected int m_nConsumerCount;

   // associations

   /**
    * The idle consumer list (used as a stack).
    */
   protected List m_idleConsumerList = new ArrayList();

   /**
    * The resource adapter.
    */
   protected GenericResourceAdapter m_adapter;

   /**
    * The consumer configuration object.
    */
   protected GenericConsumerConfig m_config;

   /**
    * The message endpoint factory.
    */
   protected MessageEndpointFactory m_factory;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Constructs the pool.
    * @param adapter The resource adapter.
    * @param factory The message endpoint factory.
    * @param logger The class logger.
    */
   protected GenericConsumerPool(GenericResourceAdapter adapter, MessageEndpointFactory factory,
      GenericConsumerConfig config, Logger logger)
   {
      m_adapter = adapter;
      m_factory = factory;
      m_config = config;
      m_logger = logger;
   }

   // operations

   /**
    * @return The resource adapter.
    */
   public GenericResourceAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * @return The consumer configuration object.
    */
   public GenericConsumerConfig getConfig()
   {
      return m_config;
   }

   /**
    * @return True if the message delivery is transactional.
    */
   public abstract boolean isTransactional();

   /**
    * @return The message endpoint factory.
    */
   public MessageEndpointFactory getFactory()
   {
      return m_factory;
   }

   /**
    * @return A new consumer instance.
    */
   protected abstract GenericConsumer createConsumer() throws Throwable;

   /**
    * @return A consumer from the pool, creating new ones if needed
    * and blocking when the maximum pool size has been reached.
    */
   protected GenericConsumer getConsumer() throws Throwable
   {
      int nMaxPoolSize = getMaxPoolSize();

      synchronized (m_idleConsumerList)
      {
         for (;;)
         {
            if (!m_idleConsumerList.isEmpty())
            {
               m_idleConsumerList.notifyAll();

               return (GenericConsumer)m_idleConsumerList.remove(m_idleConsumerList.size() - 1);
            }

            if (nMaxPoolSize >= 0 && m_nConsumerCount >= nMaxPoolSize)
            {
               if (!m_bMaxReached)
               {
                  m_bMaxReached = true;

                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Maximum size of " + nMaxPoolSize + " reached by " + this);
                  }
               }

               try
               {
                  m_idleConsumerList.wait();
               }
               catch (InterruptedException e)
               {
               }
            }
            else
            {
               ++m_nConsumerCount;
               m_idleConsumerList.notifyAll();

               break;
            }
         }
      }

      GenericConsumer consumer = null;

      try
      {
         consumer = createConsumer();
      }
      finally
      {
         if (consumer == null)
         {
            synchronized (m_idleConsumerList)
            {
               --m_nConsumerCount;
               m_idleConsumerList.notifyAll();
            }
         }
      }

      return consumer;
   }

   /**
    * @return The maximum pool size.
    */
   protected int getMaxPoolSize()
   {
      return m_config.getMaxPoolSize();
   }

   /**
    * @return True to run a synchronized listener loop.
    */
   protected boolean isSynchronized()
   {
      return true;
   }

   /**
    * Blocks waiting for incoming messages.
    */
   protected void listen()
   {
      try
      {
         wait();
      }
      catch (InterruptedException e)
      {
      }
   }

   /**
    * Interrupts listener loop.
    */
   protected void interrupt()
   {
      notifyAll();
   }

   /**
    * @see java.lang.Runnable#run()
    */
   public void run()
   {
      long lReconnectionDelay = m_config.getReconnectionDelay();

      for (;;)
      {
         boolean bShutdown;

         if (m_config.getMaxPoolSize() != 0)
         {
            if (isSynchronized())
            {
               synchronized (this)
               {
                  m_bRunning = true;

                  while (!(m_bShutdown | m_bConnect))
                  {
                     listen();
                  }

                  m_bConnect = false;
                  bShutdown = m_bShutdown;
               }
            }
            else
            {
               for (;;)
               {
                  synchronized (this)
                  {
                     m_bRunning = true;

                     if (m_bShutdown | m_bConnect)
                     {
                        m_bConnect = false;
                        bShutdown = m_bShutdown;

                        break;
                     }
                  }

                  listen();
               }
            }
         }
         else
         {
            synchronized (this)
            {
               while (!m_bShutdown && m_config.getMaxPoolSize() == 0)
               {
                  try
                  {
                     wait();
                  }
                  catch (InterruptedException e)
                  {
                  }
               }

               bShutdown = true;
            }
         }

         disconnect();

         if (bShutdown)
         {
            synchronized (this)
            {
               m_bRunning = false;
               notifyAll();
            }

            return;
         }

         try
         {
            connect();
            lReconnectionDelay = m_config.getReconnectionDelay();

            continue;
         }
         catch (Throwable e)
         {
            m_logger.error("Error in " + this, e);

            synchronized (this)
            {
               m_bConnect = true;
            }
         }

         disconnect();

         if (lReconnectionDelay > 0)
         {
            synchronized (this)
            {
               if (!m_bShutdown)
               {
                  try
                  {
                     wait(lReconnectionDelay);
                  }
                  catch (InterruptedException e)
                  {
                  }
               }
            }
         }

         lReconnectionDelay = Math.min(lReconnectionDelay << 1, 120000);
      }
   }

   /**
    * @see javax.resource.spi.work.Work#release()
    */
   public void release()
   {
   }

   /**
    * Deactivates a consumer.
    * @param consumer The consumer to deactivate.
    */
   public void deactivate(GenericConsumer consumer)
   {
      int nMaxPoolSize = getMaxPoolSize();
      List disposalList = null;

      synchronized (m_idleConsumerList)
      {
         m_idleConsumerList.add(consumer);

         while (!m_idleConsumerList.isEmpty() &&
            nMaxPoolSize >= 0 && m_nConsumerCount > nMaxPoolSize)
         {
            if (disposalList == null)
            {
               disposalList = new ArrayList(Math.min(m_idleConsumerList.size(),
                  m_nConsumerCount - nMaxPoolSize));
            }

            disposalList.add(m_idleConsumerList.remove(m_idleConsumerList.size() - 1));
         }

         m_idleConsumerList.notifyAll();
      }

      if (disposalList != null)
      {
         for (int i = 0; i < disposalList.size(); ++i)
         {
            ((GenericConsumer)disposalList.get(i)).dispose();
         }
      }
   }

   /**
    * Removes a consumer.
    * @param consumer The consumer to remove.
    */
   public void remove(GenericConsumer consumer)
   {
      synchronized (m_idleConsumerList)
      {
         --m_nConsumerCount;
         m_idleConsumerList.notifyAll();
      }
   }

   /**
    * Starts the consumer pool up.
    */
   public void startup() throws ResourceException
   {
      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Starting up " + this);
      }

      m_bConnect = true;
      m_bRunning = false;
      m_bMaxReached = false;
      m_bShutdown = false;

      if (m_config.getMaxPoolSize() != 0)
      {
         m_adapter.getContext().getWorkManager().scheduleWork(this, 60000, null, null);
      }
   }

   /**
    * Shuts the consumer pool down.
    */
   public void shutdown()
   {
      synchronized (this)
      {
         m_bShutdown = true;
         interrupt();
         notifyAll();

         while (m_bRunning)
         {
            try
            {
               wait();
            }
            catch (InterruptedException e)
            {
            }
         }
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Shut down " + this);
      }
   }

   /**
    * Shutdown the pool, update the current consumer config with new values,
    * and startup the pool again.
    * @param newConfig A consumer config object with updated properties.
    * @throws ResourceException if the operation is not supported or the pool
    *    fails to startup.
    */
   public synchronized void restart(GenericConsumerConfig newConfig) throws ResourceException
   {
      if (m_bShutdown)
      {
         return;
      }

      shutdown();

      if (newConfig != null)
      {
         m_config.update(newConfig);
      }

      startup();
   }

   /**
    * Establishes the consumer connection.
    */
   protected abstract void connect() throws Throwable;

   /**
    * Asynchronously reestablishes the consumer connection.
    */
   protected void reconnect()
   {
      synchronized (this)
      {
         m_bConnect = true;
         interrupt();
      }

      synchronized (m_idleConsumerList)
      {
         m_idleConsumerList.notifyAll();
      }
   }

   /**
    * Stops receiving messages on the consumer connection.
    */
   protected abstract void stop();

   /**
    * Releases the resources associated with the consumer connection.
    */
   protected abstract void close();

   /**
    * Disconnects from the destination.
    */
   protected void disconnect()
   {
      try
      {
         synchronized (this)
         {
            stop();
            notifyAll();
         }

         synchronized (m_idleConsumerList)
         {
            m_idleConsumerList.notifyAll();
         }

         for (;;)
         {
            GenericConsumer consumer;

            synchronized (m_idleConsumerList)
            {
               while (m_nConsumerCount != 0 && m_idleConsumerList.isEmpty())
               {
                  try
                  {
                     m_idleConsumerList.wait();
                  }
                  catch (InterruptedException e)
                  {
                  }
               }

               if (m_idleConsumerList.isEmpty())
               {
                  break;
               }

               consumer = (GenericConsumer)m_idleConsumerList.remove(m_idleConsumerList.size() - 1);
               m_idleConsumerList.notifyAll();
            }

            consumer.dispose();
         }

         synchronized (this)
         {
            close();
            notifyAll();
         }
      }
      catch (Throwable e)
      {
         m_logger.error("Unable to disconnect " + this, e);
      }
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
      buf.append("(tx=");
      buf.append(isTransactional());
      buf.append(", config=");
      buf.append(m_config);
      buf.append(')');

      return buf.toString();
   }
}
