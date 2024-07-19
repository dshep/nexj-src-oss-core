package nexj.core.rpc.timer;

import javax.transaction.xa.XAResource;

import nexj.core.util.pool.consumer.GenericConsumer;
import nexj.core.util.pool.consumer.GenericConsumerPool;

/**
 * Timer consumer.
 */
public class TimerConsumer extends GenericConsumer
{
   // associations

   /**
    * The timeout.
    */
   protected Timeout m_timeout;

   // constructors

   /**
    * Constructs the consumer.
    * @param The consumer pool. 
    */
   public TimerConsumer(GenericConsumerPool pool) throws Throwable
   {
      super(pool);
   }

   // operations

   /**
    * @see nexj.core.util.pool.consumer.Consumer#getXAResource()
    */
   public XAResource getXAResource()
   {
      return null;
   }

   /**
    * Starts the consumer on a separate thread.
    * @param timeout The timeout.
    */
   public void start(Timeout timeout) throws Throwable
   {
      m_timeout = timeout;
      activate();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#init()
    */
   protected void init() throws Throwable
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      m_timeout.add(m_timeout.getPeriod());

      consume(m_timeout);

      return false;
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#deactivate()
    */
   public void deactivate()
   {
      ((TimerConsumerPool)m_pool).schedule(m_timeout);
      m_timeout = null;
      super.deactivate();
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#reset()
    */
   public void reset()
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#drop()
    */
   protected void drop() throws Throwable
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#createInterceptor()
    */
   protected Object createInterceptor() throws Throwable
   {
      return null;
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#begin()
    */
   protected void begin() throws Throwable
   {
      m_interceptor = super.createInterceptor();
      super.begin();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#end()
    */
   protected void end() throws Throwable
   {
      try
      {
         super.end();
      }
      finally
      {
         super.releaseInterceptor();
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#releaseInterceptor()
    */
   protected void releaseInterceptor() throws Throwable
   {
      if (m_interceptor != null)
      {
         super.releaseInterceptor();
      }
   }
}
