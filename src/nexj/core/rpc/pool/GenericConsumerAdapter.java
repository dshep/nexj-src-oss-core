package nexj.core.rpc.pool;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import nexj.core.meta.integration.Channel;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.runtime.platform.generic.GenericTransactionManagerLocator;
import nexj.core.util.ObjUtil;
import nexj.core.util.pool.consumer.Consumer;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.EnlistmentException;

/**
 * Generic JCA-less consumer adapter.
 */
public class GenericConsumerAdapter implements ConsumerAdapter, StatManagerAware
{
   // associations

   /**
    * The transaction manager.
    */
   protected TransactionManager m_manager;

   /**
    * Thread pool for running listener threads.
    */
   protected ThreadPool m_listenerPool;

   /**
    * Thread pool for running work items.
    */
   protected ThreadPool m_receiverPool;

   // constructors

   /**
    * Constructs the consumer adapter.
    */
   public GenericConsumerAdapter()
   {
      try
      {
         m_manager = new GenericTransactionManagerLocator().getTransactionManager();
         m_listenerPool = new ThreadPool("Listener Thread Pool");
         m_receiverPool = new ThreadPool("Receiver Thread Pool");
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
      m_listenerPool.setStatManager(statManager);
      m_receiverPool.setStatManager(statManager);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#getTransactionManager()
    */
   public TransactionManager getTransactionManager()
   {
      return m_manager;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#run(java.lang.Runnable)
    */
   public void run(Runnable runnable) throws Throwable
   {
      ((ThreadPool.Worker)m_listenerPool.get(null)).run(runnable);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#run(nexj.core.util.pool.consumer.Consumer)
    */
   public void run(Consumer consumer) throws Throwable
   {
      ((ThreadPool.Worker)m_receiverPool.get(null)).run(consumer);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#getInterceptor(nexj.core.util.pool.consumer.Consumer)
    */
   public Object getInterceptor(Consumer consumer) throws Throwable
   {
      return null;
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#begin(nexj.core.util.pool.consumer.Consumer, java.lang.Object)
    */
   public void begin(Consumer consumer, Object interceptor) throws Throwable
   {
      XAResource xar = consumer.getXAResource();

      if (xar != null)
      {
         Transaction tx = m_manager.getTransaction();

         if (tx == null)
         {
            m_manager.begin();

            try
            {
               tx = m_manager.getTransaction();
            }
            catch (Throwable t)
            {
               m_manager.rollback();

               throw t;
            }
         }

         try
         {
            if (!tx.enlistResource(xar))
            {
               throw new EnlistmentException();
            }
         }
         catch (Throwable t)
         {
            m_manager.rollback();

            throw t;
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#process(nexj.core.util.pool.consumer.Consumer, java.lang.Object, java.lang.Object)
    */
   public void process(Consumer consumer, Object interceptor, Object request) throws Throwable
   {
      ((Processor)((Channel)consumer.getConfig()).getReceiver().getInstance(null)).process(request);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#end(java.lang.Object)
    */
   public void end(Object interceptor) throws Throwable
   {
      // No need to deal with the transaction here since the pool takes care of it
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#releaseInterceptor(java.lang.Object)
    */
   public void releaseInterceptor(Object interceptor) throws Throwable
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#dispose()
    */
   public void dispose()
   {
      m_listenerPool.dispose();
      m_receiverPool.dispose();
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#maintain()
    */
   public void maintain()
   {
      m_listenerPool.maintain();
      m_receiverPool.maintain();
   }
}
