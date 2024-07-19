package nexj.core.rpc.queueing.ra;

import java.lang.reflect.Method;
import java.util.Arrays;

import javax.resource.spi.endpoint.MessageEndpoint;

import nexj.core.rpc.queueing.DispatcherMessage;
import nexj.core.rpc.queueing.ObjectDispatchListener;
import nexj.core.rpc.queueing.ObjectQueueing;
import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Consumer of DispatcherMessages.
 */
public class ObjectConsumer extends GenericConsumer
{
   // associations

   protected DispatcherMessage m_message;

   /**
    * The UDPListener.onMessage() method object.
    */
   protected final static Method LISTENER_ONMESSAGE_METHOD = getMethod(ObjectDispatchListener.class,
      "onMessage", new Class[]{DispatcherMessage.class});

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectConsumer.class);

   // constructors

   /**
    * Constructs the consumer.
    * @param pool The consumer pool.
    */
   public ObjectConsumer(ObjectConsumerPool pool) throws Throwable
   {
      super(pool, s_logger);
   }

   /**
    * Starts the consumer to process a message.
    * @param message The message to process.
    */
   public void start(DispatcherMessage message) throws Throwable
   {
      m_message = message;

      activate();
   }

   /**
    * Prepares the consumer to process a message.
    * @param socket The socket from which to read the message.
    * @param sDispatcher The current dispatcher.
    * @param bThreadRequired True if the consumer must be activated, false if run can be called directly.
    */
   public void start(final ObjectSocket socket, final String sDispatcher, boolean bThreadRequired) throws Throwable
   {
      final ObjectConsumerPool pool = (ObjectConsumerPool)m_pool;
      boolean bReleased = false;

      try
      {
         m_message = new DispatcherMessageDelegator()
         {
            public DispatcherMessage m_message;

            public DispatcherMessage getMessage()
            {
               try
               {
                  if (m_message == null)
                  {
                     m_message = (DispatcherMessage)socket.read();

                     if (s_logger.isDebugEnabled())
                     {
                        s_logger.debug("Handling request to dispatcher (" + Integer.toString(m_message.getType()) +
                           ", " + Arrays.toString(m_message.getArgArray()) + ")");
                     }
                  }
               }
               catch (Throwable t)
               {
                  if (pool.isStopped())
                  {
                     m_message = new DispatcherMessage()
                     {
                        public void respond(Object result)
                        {
                        }

                        public int getType()
                        {
                           return WAKE;
                        }

                        public String getDispatcherName()
                        {
                           return sDispatcher;
                        }

                        public Object[] getArgArray()
                        {
                           return new Object[]{new Long(System.currentTimeMillis())};
                        }

                        public void error(int nCause, Throwable t)
                        {
                        }
                     };
                  }
                  else
                  {
                     ObjUtil.rethrow(t);
                  }
               }

               return m_message;
            }

            public void respond(Object result)
            {
               if (getType() == WAKE)
               {
                  pool.wake(((Long)getArgArray()[0]));
                  pool.respond(socket, null);
               }
               else
               {
                  pool.respond(socket, result);
               }
            }

            public void error(int nCause, Throwable t)
            {
               if (nCause == DISPATCHER_CHANGED)
               {
                  pool.invalidateDispatcher();
               }

               if (nCause == SHUTDOWN)
               {
                  pool.setStopped(true);
               }

               pool.respond(socket, null);
            }

            public String getDispatcherName()
            {
               return sDispatcher;
            }
         };

         if (bThreadRequired)
         {
            activate();
         }
         else
         {
            try
            {
               run();
            }
            finally
            {
               deactivate();
            }
         }

         bReleased = true;
      }
      catch (Throwable t)
      {
         if (!bReleased)
         {
            pool.respond(socket, null);
         }
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#run()
    */
   public void run()
   {
      boolean bEnabled = Logger.isEnabled();

      try
      {
         ObjectQueueing.LOGGER.enable();
         super.run();
      }
      finally
      {
         Logger.setEnabled(bEnabled);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deactivate()
    */
   protected void deactivate()
   {
      boolean bEnabled = Logger.isEnabled();

      try
      {
         ObjectQueueing.LOGGER.enable();
         super.deactivate();
      }
      finally
      {
         Logger.setEnabled(bEnabled);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#close()
    */
   protected void close()
   {
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#consume()
    */
   protected boolean consume() throws Throwable
   {
      consume(m_message);

      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deliver(java.lang.Object)
    */
   protected void deliver(Object message) throws Throwable
   {
      DispatcherMessage msg = (DispatcherMessage)message;

      ((ObjectDispatchListener)m_endpoint).onMessage(msg);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#getMethod()
    */
   protected Method getMethod()
   {
      return LISTENER_ONMESSAGE_METHOD;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#init()
    */
   protected MessageEndpoint init() throws Throwable
   {
      return m_pool.getFactory().createEndpoint(null);
   }
}
