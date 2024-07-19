package nexj.core.util.pool.consumer;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Generic request consumer implementation.
 */
public abstract class GenericConsumer implements Consumer
{
   // attributes

   /**
    * True if the request processing has failed.
    */
   protected boolean m_bFailed;

   /**
    * Dispose of the consumer after processing the request.
    */
   protected boolean m_bDispose;

   /**
    * True if the consumer has been initialized. 
    */
   protected boolean m_bInitialized;

   /**
    * The time of last use in milliseconds since 1-Jan-1970 UTC.
    */
   protected long m_lTime;

   // associations

   /**
    * The consumer pool.
    */
   protected GenericConsumerPool m_pool;

   /**
    * The consumer configuration.
    */
   protected ConsumerConfig m_config;

   /**
    * The request interceptor.
    */
   protected Object m_interceptor;

   /**
    * The class logger.
    */
   protected Logger m_logger;

   // constructors

   /**
    * Constructs the consumer.
    * @param pool The consumer pool.
    * @param logger The class logger. Can be null to use a default logger.
    */
   protected GenericConsumer(GenericConsumerPool pool, Logger logger) throws Throwable
   {
      m_pool = pool;
      m_config = pool.getConfig();
      m_logger = (logger != null) ? logger : pool.getLogger();

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Initializing " + this);
      }

      try
      {
         init();
         m_interceptor = createInterceptor();
         m_bInitialized = true;
      }
      catch (Throwable t)
      {
         dispose();

         throw t;
      }
   }

   /**
    * Constructs the consumer with a default logger.
    * @param pool The consumer pool.
    */
   protected GenericConsumer(GenericConsumerPool pool) throws Throwable
   {
      this(pool, null);
   }

   // operations
   
   /**
    * Sets the time of last use in milliseconds since 1-Jan-1970 UTC.
    * @param lTime The time to set.
    */
   public void setTime(long lTime)
   {
      m_lTime = lTime;
   }

   /**
    * @return The time of last use in milliseconds since 1-Jan-1970 UTC, or 0 if not set.
    */
   public long getTime()
   {
      return m_lTime;
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#getConfig()
    */
   public ConsumerConfig getConfig()
   {
      return m_config;
   }

   /**
    * Initializes the consumer.
    */
   protected abstract void init() throws Throwable;

   /**
    * Creates an interceptor.
    * @see ConsumerAdapter#getInterceptor(Consumer)
    */
   protected Object createInterceptor() throws Throwable
   {
      return m_pool.getAdapter().getInterceptor(this);
   }

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
         m_pool.getAdapter().run(this);
      }
      catch (Throwable t)
      {
         if (m_logger.isWarnEnabled())
         {
            m_logger.warn("Unable to run " + this, t);
         }

         throw t;
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#run()
    */
   public void run()
   {
      Logger enabler = m_pool.getEnabler();
      boolean bLogEnabledSaved = false;
      boolean bXA = false;
      Throwable t = null;

      m_bFailed = false;
      m_bDispose = false;

      if (enabler != null)
      {
         bLogEnabledSaved = Logger.isEnabled();
         enabler.enable();
      }

      try
      {
         begin();

         try
         {
            bXA = consume();
         }
         catch (Throwable e)
         {
            m_bFailed = true;
            throw e;
         }
         finally
         {
            try
            {
               end();
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
      else
      {
         deactivate();
      }

      if (enabler != null)
      {
         Logger.setEnabled(bLogEnabledSaved);
      }
   }

   /**
    * Begins processing the request.
    * @see ConsumerAdapter#begin(Consumer, Object)
    */
   protected void begin() throws Throwable
   {
      m_pool.getAdapter().begin(this, m_interceptor);
   }

   /**
    * Consumes all the requests assigned to this consumer in this activation.
    * Invokes, directly or indirectly, consume(Object).
    * @see GenericConsumer#consume(Object)
    * @return True if an XA transaction has been started.
    */
   protected abstract boolean consume() throws Throwable;

   /**
    * Ends processing the request.
    * @see ConsumerAdapter#end(Object)
    */
   protected void end() throws Throwable
   {
      m_pool.getAdapter().end(m_interceptor);
   }

   /**
    * Consumes a request.
    * @param request The request to consume.
    */
   protected void consume(Object request)
   {
      try
      {
         process(request);
      }
      catch (Throwable e)
      {
         m_bFailed = true;

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Request processing error in " + this, e);
         }
      }
   }

   /**
    * Processes the request. Invoked by consume(Object).
    * @see GenericConsumer#consume(Object)
    * @param request The request to process.
    */
   protected void process(Object request) throws Throwable
   {
      m_pool.getAdapter().process(this, m_interceptor, request);
   }

   /**
    * Template method invoked from run() to finish the session processing.
    * @see GenericConsumer#run()
    * @param bXA True if an XA transaction has been started.
    */
   protected void finish(boolean bXA)
   {
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#deactivate()
    */
   public void deactivate()
   {
      if (m_pool != null)
      {
         try
         {
            reset();
         }
         catch (Throwable t)
         {
            m_logger.error("Unable to reset " + this, t);
            dispose();

            return;
         }

         if (m_pool.getConfig() != m_config)
         {
            dispose();

            return;
         }

         m_pool.deactivate(this);

         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Deactivated " + this);
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.Consumer#dispose()
    */
   public void dispose()
   {
      if (m_bInitialized)
      {
         try
         {
            releaseInterceptor();
         }
         catch (Throwable t)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Unable to dispose of " + this, t);
            }
         }
      }

      try
      {
         drop();
      }
      catch (Throwable t)
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Unable to dispose of " + this, t);
         }
      }

      if (m_bInitialized)
      {
         m_pool.remove(this);
         m_bInitialized = false;
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Disposed of " + this);
      }
   }

   /**
    * Releases the interceptor.
    * @see ConsumerAdapter#releaseInterceptor(Object)
    */
   protected void releaseInterceptor() throws Throwable
   {
      m_pool.getAdapter().releaseInterceptor(m_interceptor);
   }

   /**
    * Releases any physical resources.
    */
   protected abstract void drop() throws Throwable;

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
      buf.append(m_config);
      buf.append(')');

      return buf.toString();
   }
}
