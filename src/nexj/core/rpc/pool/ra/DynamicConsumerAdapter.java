package nexj.core.rpc.pool.ra;

import java.lang.reflect.Method;
import java.security.AccessController;
import java.security.PrivilegedAction;

import javax.resource.spi.BootstrapContext;
import javax.resource.spi.endpoint.MessageEndpoint;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.resource.spi.work.ExecutionContext;
import javax.resource.spi.work.Work;
import javax.transaction.TransactionManager;

import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.rpc.pool.ThreadPool;
import nexj.core.util.Cancellable;
import nexj.core.util.ObjUtil;
import nexj.core.util.pool.consumer.Consumer;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerConfig;

/**
 * Consumer adapter for a Java EE container. 
 */
public class DynamicConsumerAdapter implements ConsumerAdapter, StatManagerAware
{
   // constants

   /**
    * Processor.process(Object, ConsumerConfig) method.
    */
   protected final static Method PROCESS_METHOD = getMethod(DynamicProcessor.class,
      "process", new Class[]{Object.class, ConsumerConfig.class});

   // associations

   /**
    * The container context.
    */
   protected BootstrapContext m_context;

   /**
    * The transaction manager.
    */
   protected TransactionManager m_manager;

   /**
    * The RA message non-transactional endpoint factory.
    */
   protected MessageEndpointFactory m_factory;

   /**
    * The RA message transactional endpoint factory.
    */
   protected MessageEndpointFactory m_txFactory;

   /**
    * Thread pool for running listener threads.
    */
   protected ThreadPool m_listenerPool;

   // constructors

   /**
    * Constructs the consumer adapter.
    * @param context The container context.
    * @param manager The transaction manager.
    * @param factory The non-transactional endpoint factory.
    * @param txFactory The transactional endpoint factory.
    */
   public DynamicConsumerAdapter(BootstrapContext context, TransactionManager manager,
      MessageEndpointFactory factory, MessageEndpointFactory txFactory)
   {
      m_context = context;
      m_manager = manager;
      m_factory = factory;
      m_txFactory = txFactory;
      m_listenerPool = new ThreadPool("Listener Thread Pool");
   }

   // operations

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
      m_listenerPool.setStatManager(statManager);
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
      ExecutionContext xctx = null;
      int nTimeout = consumer.getConfig().getTransactionTimeout();

      if (nTimeout != 0)
      {
         xctx = new ExecutionContext();
         xctx.setTransactionTimeout((nTimeout < 0) ? Long.MAX_VALUE : nTimeout);
      }

      m_context.getWorkManager().scheduleWork(new RunnableWork(consumer), 10000, xctx, null);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#getInterceptor(nexj.core.util.pool.consumer.Consumer)
    */
   public Object getInterceptor(Consumer consumer) throws Throwable
   {
      return ((consumer.getConfig().isTransactional()) ? m_txFactory : m_factory).createEndpoint(consumer.getXAResource());
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#begin(Consumer, java.lang.Object)
    */
   public void begin(Consumer consumer, Object interceptor) throws Throwable
   {
      ((MessageEndpoint)interceptor).beforeDelivery(PROCESS_METHOD);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#process(Consumer, java.lang.Object, java.lang.Object)
    */
   public void process(Consumer consumer, Object interceptor, Object request) throws Throwable
   {
      ((DynamicProcessor)interceptor).process(request, consumer.getConfig());
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#end(java.lang.Object)
    */
   public void end(Object interceptor) throws Throwable
   {
      ((MessageEndpoint)interceptor).afterDelivery();
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#releaseInterceptor(java.lang.Object)
    */
   public void releaseInterceptor(Object interceptor) throws Throwable
   {
      ((MessageEndpoint)interceptor).release();
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

   // inner classes

   /**
    * Runnable wrapper.
    */
   public static class RunnableWork implements Work
   {
      // associations

      /**
       * The wrapped runnable.
       */
      protected Runnable m_runnable;

      // constructors

      /**
       * Constructs the wrapper.
       * @param runnable The runnable to wrap.
       */
      public RunnableWork(Runnable runnable)
      {
         m_runnable = runnable;
      }

      // operations

      /**
       * @see java.lang.Runnable#run()
       */
      public void run()
      {
         m_runnable.run();
      }

      /**
       * @see javax.resource.spi.work.Work#release()
       */
      public void release()
      {
         if (m_runnable instanceof Cancellable)
         {
            ((Cancellable)m_runnable).cancel();
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return ObjUtil.getShortClassName(this) + '(' + m_runnable + ')';
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#dispose()
    */
   public void dispose()
   {
      m_listenerPool.dispose();
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerAdapter#maintain()
    */
   public void maintain()
   {
      m_listenerPool.maintain();
   }
}