package nexj.core.rpc.queueing.ra;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketTimeoutException;
import java.sql.Timestamp;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.resource.ResourceException;
import javax.resource.spi.endpoint.MessageEndpointFactory;
import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkEvent;
import javax.resource.spi.work.WorkListener;

import nexj.core.rpc.TransferObject;
import nexj.core.rpc.queueing.DispatcherMessage;
import nexj.core.rpc.queueing.ObjectQueueing;
import nexj.core.rpc.ra.GenericConsumer;
import nexj.core.rpc.ra.GenericConsumerPool;
import nexj.core.util.Executable;
import nexj.core.util.HashHolder;
import nexj.core.util.IdentityHashHolder;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.RandUtil;

/**
 * Consumer pool for ObjectQueue adapter.
 */
public class ObjectConsumerPool extends GenericConsumerPool
{
   // constants

   /**
    * The number of times to retry a method invocation over the network.
    */
   protected static int INVOKE_TRIES = 20;

   /**
    * Timeout for getMessage message invocations, in milliseconds.
    */
   public static Long GET_MESSAGE_TIMEOUT = Long.valueOf(60000L);

   // associations

   /**
    * The dispatcher state: all information relating to object dispatching.
    */
   protected DispatcherState m_state;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(ObjectConsumerPool.class);

   // constructors

   /**
    * Constructs the consumer pool.
    * @param adapter The resource adapter.
    * @param factory The endpoint factory.
    * @param config The pool configuration.
    * @throws ResourceException if an initialization error occurs.
    */
   public ObjectConsumerPool(ObjectQueueResourceAdapter adapter, MessageEndpointFactory factory, ObjectConsumerConfig config)
      throws ResourceException
   {
      super(adapter, factory, config, s_logger);
   }

   // operations

   /**
    * Starts the consumer pool up.
    */
   public void startup() throws ResourceException
   {
      if (m_state == null)
      {
         m_state = createState();
      }

      ((ObjectQueueResourceAdapter)m_adapter).putConsumerPool(((ObjectConsumerConfig)m_config).getPort(), this);

      super.startup();
   }

   /**
    * @return the consumer pool state
    */
   protected DispatcherState createState()
   {
      return new DispatcherState();
   }

   /**
    * Shuts the consumer pool down.
    */
   public void shutdown()
   {
      super.shutdown();

      ((ObjectQueueResourceAdapter)m_adapter).removeConsumerPool(((ObjectConsumerConfig)m_config).getPort());
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#close()
    */
   protected void close()
   {
      m_state.close();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#interrupt()
    */
   protected void interrupt()
   {
      close();
      notifyAll();
   }

   /**
    * Get a consumer from this pool.
    * @return the consumer.
    */
   public ObjectConsumer getObjectConsumer() throws Throwable
   {
      return (ObjectConsumer)getConsumer();
   }

   /**
    * Wake the dispatcher at a given time.
    * @param timeout the time at which to wake the dispatcher.
    */
   public void wake(Long timeout)
   {
      m_state.wake(timeout);
   }

   /**
    * Wake the dispatcher.
    */
   public void wake()
   {
      m_state.wake(new Long(System.currentTimeMillis()));
   }

   /**
    * Posts a command for delivery to one or several nodes.
    * @param tobj the serializable message to post.
    */
   public void post(TransferObject tobj)
   {
      m_state.post(tobj);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#listen()
    */
   protected void listen()
   {
      m_state.listen();
   }

   /**
    * @return String the dispatcher node name.
    */
   public String getDispatcherNodeName()
   {
      return m_state.getDispatcherNodeName();
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new ObjectConsumer(this);
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPoo
    */
   protected boolean isSynchronized()
   {
      return false;
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#stop()
    */
   protected void stop()
   {
      interrupt();
   }

   /**
    * Post a command to the cluster.
    *
    * @param nPriority the command priority.
    * @param sNode a node name.
    * @param nDistribution the distribution category, one of the constants from ObjectDispatchListener:
    *       ANY - command must be processed by any one node.
    *       ALL - command must be processed by all nodes.
    *       ONLY - command must be processed by sNode.
    *       EXCEPT - command must be processed by all nodes except sNode.
    *       DISPATCHER - command must be processed synchronously by the dispatcher node.
    * @param command the command to deliver.
    */
   public void post(int nPriority, String sNode, int nDistribution, Executable command)
   {
      m_state.invoke(DispatcherMessage.POST, new Object[]
      {
         Integer.valueOf(nPriority),
         sNode,
         Integer.valueOf(nDistribution),
         command
      }, true);
   }

   /**
    * Notify the pool of the dispatcher component state.
    *
    * @param bStopped true if the dispatcher component has stopped.
    */
   public void setStopped(boolean bStopped)
   {
      m_state.setStopped(bStopped);
   }

   /**
    * @return boolean true if the dispatcher component is stopped.
    */
   public boolean isStopped()
   {
      return m_state.isStopped();
   }

   /**
    * Notify the pool that the dispatcher has changed.
    */
   public void invalidateDispatcher()
   {
      m_state.invalidateDispatcher();
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectQueueConnection#revoke(java.util.List)
    */
   public void revoke(List unavailableNodeList)
   {
   }

   /**
    * Return a response and return the socket to the pool.
    *
    * @param socket the socket to which to respond.
    * @param result the result.
    */
   public void respond(ObjectSocket socket, Object result)
   {
      try
      {
         MessageResponse response = null;

         if (result instanceof DispatcherMessage)
         {
            DispatcherMessage resultMessage = (DispatcherMessage)result;

            response = new MessageResponse(new SerializableMessage(resultMessage));
         }
         else
         {
            response = MessageResponse.NONE;
         }

         socket.write(response);
      }
      catch (Throwable t)
      {
      }
      finally
      {
         try
         {
            socket.close();
         }
         catch (Throwable tt)
         {
            s_logger.warn("Failed to close receiving socket", tt);
         }

         m_state.removeSocket(socket);
      }
   }

   // inner classes

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#run()
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

   protected abstract class RepeatedWork implements Work, WorkListener
   {
      public boolean m_bRunning = true;

      public String m_sName;

      public RepeatedWork(String sName)
      {
         m_sName = sName;
      }

      /**
       * signal the thread to stop.
       */
      public void stop()
      {
         synchronized (ObjectConsumerPool.this.m_state)
         {
            s_logger.debug("Stopping listener thread: " + m_sName);
            m_bRunning = false;
            ObjectConsumerPool.this.m_state.notifyAll();
         }

         s_logger.debug("Stopped listener thread: " + m_sName);
      }

      /**
       * Relaunch the thread.
       */
      public void restart()
      {
         synchronized (ObjectConsumerPool.this.m_state)
         {
            if (m_bRunning)
            {
               s_logger.debug("Renewing listener thread: " + m_sName);

               try
               {
                  getAdapter().getContext().getWorkManager().scheduleWork(this, 10000, null, this);
               }
               catch (Throwable t)
               {
                  s_logger.warn("Failed to relaunch listener thread: " + m_sName);
                  invalidateDispatcher();
               }
            }
            else
            {
               s_logger.debug("Exiting listener thread: " + m_sName);
            }
         }
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
      public void workAccepted(WorkEvent arg0)
      {
      }

      /**
       * @see javax.resource.spi.work.WorkListener#workCompleted(javax.resource.spi.work.WorkEvent)
       */
      public void workCompleted(WorkEvent arg0)
      {
         boolean bEnabled = Logger.isEnabled();

         try
         {
            ObjectQueueing.LOGGER.enable();
            restart();
         }
         finally
         {
            Logger.setEnabled(bEnabled);
         }
      }

      /**
       * @see javax.resource.spi.work.WorkListener#workRejected(javax.resource.spi.work.WorkEvent)
       */
      public void workRejected(WorkEvent arg0)
      {
         restart();
      }

      /**
       * @see javax.resource.spi.work.WorkListener#workStarted(javax.resource.spi.work.WorkEvent)
       */
      public void workStarted(WorkEvent arg0)
      {
      }
   }

   /**
    * All information relating to object dispatching. Many operation must
    * synchronized on this data, so it must be embedded in a single instance.
    */
   protected class DispatcherState
   {
      // attributes

      /**
       * The distributed flag.
       */
      public boolean m_bDistributed;

      /**
       * The node name.
       */
      public String m_sNodeName = J2EEUtil.ISOLATED_NODE_NAME;

      /**
       * The dispatcher's node name.
       */
      public String m_sDispatcherNodeName;

      /**
       * The dispatcher server socket address, null if not used.
       */
      protected InetSocketAddress m_dispatcherServerAddress;

      /**
       * True if dispatcher is known.
       */
      public boolean m_bDispatcherKnown;

      /**
       * True if m_sNodeName = m_sDispatcherNodeName.
       */
      public boolean m_bDispatcher;

      /**
       * The next timeout in ms since 1970-01-01 00:00:00 UTC.
       */
      public long m_lTimeout;

      /**
       * The receiving (inbound) sockets. Null unless this node is the
       * dispatcher.
       */
      public Set m_receivingSocketSet;

      /**
       * The outbound sockets, used and unused.
       */
      public Set m_socketSet;

      /**
       * True if starting
       */
      public boolean m_bStarting;

      /**
       * True if the dispatcher component has been shutdown.
       */
      public boolean m_bDispatcherStopped;

      /**
       * True if the pool has shutdown. We limit access to m_bShutdown to reduce
       * the opportunity for deadlock.
       */
      public boolean m_bPoolShutdown;

      // associations

      /**
       * The socket acceptor work instance.
       */
      public RepeatedWork m_socketAcceptor;

      /**
       * The message pulling work instance.
       */
      public RepeatedWork m_messagePuller;

      /**
       * The listening socket. Null unless this node is the dispatcher.
       */
      public ObjectServerSocket m_serverSocket;

      // operations

      /**
       * @see nexj.core.rpc.ra.GenericConsumerPool#close()
       */
      protected void close()
      {
         synchronized (ObjectConsumerPool.this)
         {
            synchronized (this)
            {
               m_bPoolShutdown = m_bShutdown;
               notifyAll();
            }
         }

         synchronized (this)
         {
            if (m_messagePuller != null)
            {
               m_messagePuller.stop();
               m_messagePuller = null;
            }
   
            if (m_socketAcceptor != null)
            {
               try
               {
                  m_serverSocket.close();
               }
               catch (IOException e)
               {
                  s_logger.warn("Failed to close listening socket", e);
               }
   
               m_socketAcceptor.stop();
               m_socketAcceptor = null;
            }
   
            if (m_socketSet != null)
            {
               for (Iterator itr = m_socketSet.iterator(); itr.hasNext();)
               {
                  try
                  {
                     ObjectSocket socket = (ObjectSocket)itr.next();
   
                     if (!socket.isClosed())
                     {
                        socket.close();
                     }
                  }
                  catch (IOException e)
                  {
                     s_logger.warn("Failed to close outbound socket", e);
                  }
               }
            }
   
            // close incoming connections
            if (m_receivingSocketSet != null)
            {
               for (Iterator itr = m_receivingSocketSet.iterator(); itr.hasNext();)
               {
                  try
                  {
                     ObjectSocket socket = (ObjectSocket)itr.next();
   
                     if (!socket.isClosed())
                     {
                        socket.close();
                     }
                  }
                  catch (IOException e)
                  {
                     s_logger.warn("Failed to close outbound socket", e);
                  }
               }
            }
   
            m_socketSet = null;
            m_receivingSocketSet = null;
         }
      }

      /**
       * Contacts the dispatcher, assuming the responsibility if appropriate.
       */
      protected void startDispatcher()
      {
         ObjectConsumer consumer = null;

         try
         {
            s_logger.debug("Dispatcher unknown, initializing node");
            consumer = (ObjectConsumer)getConsumer();

            synchronized (this)
            {
               m_bStarting = true;

               consumer.start(new DispatcherMessage()
               {
                  public void respond(Object result)
                  {
                     synchronized (DispatcherState.this)
                     {
                        try
                        {
                           connectDispatcher(result);
                           m_bDispatcherKnown = true;
                           m_lTimeout = System.currentTimeMillis(); // dispatch immediately

                           if (m_sNodeName.equals(m_sDispatcherNodeName))
                           {
                              m_bDispatcher = true;
                              socketAccept();
                           }
                           else
                           {
                              m_bDispatcher = false;
                           }
                        }
                        catch (Throwable t)
                        {
                           m_bDispatcherKnown = false;
                           s_logger.error("Failed to connect to dispatcher", t);
                        }
                        finally
                        {
                           m_bStarting = false;
                           DispatcherState.this.notifyAll();
                        }
                     }
                  }

                  public int getType()
                  {
                     return START_NODE;
                  }

                  public Object[] getArgArray()
                  {
                     return new Object[]
                     {
                        (m_dispatcherServerAddress == null) ? null : m_dispatcherServerAddress.getAddress()
                     };
                  }

                  public void error(int nCause, Throwable t)
                  {
                     synchronized (DispatcherState.this)
                     {
                        m_bStarting = false;
                        DispatcherState.this.notifyAll();
                     }
                  }

                  public String getDispatcherName()
                  {
                     return null; // dispatcher is not yet known
                  }
               });

               consumer = null;

               while (m_bStarting && !m_bPoolShutdown)
               {
                  wait(); // wait for startup
               }
            }
         }
         catch (Throwable t)
         {
            s_logger.error("Error in " + this, t);
         }
         finally
         {
            if (consumer != null)
            {
               consumer.dispose();
            }
         }
      }

      /**
       * Launch socket accepting thread.
       */
      protected synchronized void socketAccept()
      {
         m_serverSocket = getServerSocket();

         if (m_serverSocket == null)
         {
            return;
         }

         final ObjectServerSocket serverSocket = m_serverSocket;

         m_socketAcceptor = new RepeatedWork("socket-acceptor")
         {
            public void run()
            {
               boolean bEnabled = Logger.isEnabled();

               try
               {
                  ObjectQueueing.LOGGER.enable();

                  // accept a socket
                  ObjectSocket socket = serverSocket.accept(60000);

                  // process the message from the client
                  s_logger.debug("Receiving client connection");
                  receiveRequest(socket);
               }
               catch (SocketTimeoutException e)
               {
                  s_logger.debug("Server socket accept timed out");
               }
               catch (Throwable t)
               {
                  synchronized (DispatcherState.this)
                  {
                     if (!m_bDispatcherStopped && !m_bPoolShutdown)
                     {
                        s_logger.error("Error in " + this, t);
                        invalidateDispatcher();
                     }
                  }
               }
               finally
               {
                  Logger.setEnabled(bEnabled);
               }
            }
         };

         m_socketAcceptor.restart();
      }

      /**
       * Connect to the dispatching node.
       */
      protected void connectDispatcher(Object conf)
      {
         m_sDispatcherNodeName = J2EEUtil.ISOLATED_NODE_NAME;
         m_socketSet = new IdentityHashHolder();
      }

      /**
       * Inbound dispatcher message processing loop.
       */
      protected void processMessages()
      {
         ObjectConsumer consumer = null;
         String sDispatcherNodeName = null;
         boolean bDispatch = false;

         try
         {
            synchronized (this)
            {
               long lTime = System.currentTimeMillis();

               sDispatcherNodeName = m_sDispatcherNodeName;

               while (lTime < m_lTimeout && !m_bPoolShutdown)
               {
                  wait(m_lTimeout - lTime);
                  lTime = System.currentTimeMillis();
               }

               if (m_bPoolShutdown || !m_bDispatcherKnown || !m_sDispatcherNodeName.equals(m_sNodeName))
               {
                  if (lTime >= m_lTimeout)
                  {
                     m_lTimeout = Long.MAX_VALUE;
                  }

                  return;
               }

               revoke(); // process any pending node revocations

               if (lTime >= m_lTimeout)
               {
                  m_lTimeout = Long.MAX_VALUE;
                  bDispatch = true;
               }
            }

            if (bDispatch)
            {
               final String sFinalDispatcherNodeName = sDispatcherNodeName;

               // don't need to dispatch again until another thread requests it.
               s_logger.debug("Invoking dispatch");

               // run the dispatcher
               consumer = (ObjectConsumer)getConsumer();
               consumer.start(new DispatcherMessage()
               {
                  public void respond(Object result)
                  {
                     if (result != null)
                     {
                        wake(new Long(((Timestamp)result).getTime()));
                     }
                  }

                  public int getType()
                  {
                     return DISPATCH;
                  }

                  public Object[] getArgArray()
                  {
                     return null;
                  }

                  public void error(int nCause, Throwable t)
                  {
                     if (nCause == SHUTDOWN)
                     {
                        setStopped(true);
                     }

                     if (nCause == DISPATCHER_CHANGED)
                     {
                        invalidateDispatcher();
                     }
                  }

                  public String getDispatcherName()
                  {
                     return sFinalDispatcherNodeName;
                  }
               });
               consumer = null;
            }
         }
         catch (InterruptedException e)
         {
            s_logger.debug("Dispatcher listen thread interrupted", e);
            interrupt();
         }
         catch (Throwable t)
         {
            s_logger.error("Error in " + this, t);
            ObjUtil.rethrow(t);
         }
         finally
         {
            if (consumer != null)
            {
               consumer.dispose();
            }
         }
      }

      /**
       * Creates and returns a server socket bound to the dispatcher's address
       * and port.
       * @return the server socket.
       */
      protected synchronized ObjectServerSocket getServerSocket()
      {
         if (m_bPoolShutdown)
         {
            return null;
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Dispatcher listening for requests");
         }

         return new LocalObjectServerSocket(getMaxPoolSize());
      }

      /**
       * Wakes this pool.
       * @param lTimeout The time at which to wake this pool.
       */
      protected synchronized void wake(long lTimeout)
      {
         s_logger.dump("Pool received wake request");

         if (lTimeout < m_lTimeout)
         {
            s_logger.debug("Waking pool at " + new Timestamp(lTimeout));

            m_lTimeout = lTimeout;
            notifyAll(); // wake all threads
         }
      }

      /**
       * Wake the dispatcher at a given time.
       * @param timeout the time at which to wake the dispatcher.
       */
      protected void wake(Long timeout)
      {
         synchronized (this)
         {
            if (!m_bDispatcherKnown)
            {
               return;
            }

            // don't tie up threads on the dispatcher node just to send wake
            // requests.
            if (m_bDispatcher)
            {
               wake(timeout.longValue());

               return;
            }
         }

         invoke(DispatcherMessage.WAKE, new Object[]
         {
            timeout
         }, true);
      }

      /**
       * Posts a command for delivery to one or several nodes.
       * @param tobj the serializable message to post.
       */
      public void post(TransferObject tobj)
      {
         String sDispatcherNodeName;

         synchronized (this)
         {
            sDispatcherNodeName = m_sDispatcherNodeName;
         }

         if (sDispatcherNodeName != null)
         {
            invoke(DispatcherMessage.POST, new Object[] {tobj, sDispatcherNodeName}, true);
         }
      }

      /**
       * Invoke a message on the (possibly remote) dispatcher.
       * @param int nType the type of the message.
       * @param Object[] args the arguments of the message.
       * @param bClient true if the request is from a client of the resource adapter.
       * @return Object the result of the call.
       */
      protected Object invoke(final int nType, final Object[] args, boolean bClient)
      {
         ObjectSocket socket = null;
         Throwable lastThrowable = null;
         int nInvokeTries = (bClient) ? INVOKE_TRIES : 1;
         long lDelay = 10; // in milliseconds, delay following a failed attempt

         for (int i = 1; i <= nInvokeTries; i++)
         {
            try
            {
               s_logger.dump("Invoke with type " + nType);
               socket = getSocket(bClient);

               if (socket == null)
               {
                  s_logger.debug("Invoke unable to acquire socket, returning null");

                  return null;
               }

               socket.write(new SerializableMessage(nType, args, m_sDispatcherNodeName));

               return ((MessageResponse)socket.read()).getResponse();
            }
            catch (Throwable t)
            {
               try
               {
                  if (socket != null && !socket.isClosed())
                  {
                     socket.close();
                     socket = null;
                  }
               }
               catch (Exception ee)
               {
               }

               lastThrowable = t;

               if (s_logger.isDebugEnabled() && !m_bPoolShutdown)
               {
                  s_logger.debug("Failed to invoke dispatcher on attempt " + i, t);
               }
            }
            finally
            {
               if (socket != null)
               {
                  release(socket);
               }
            }

            // delay with exponential back-off + random deviation, to a maximum
            // of 10 seconds * (0.5 to 1.5)
            if (i < nInvokeTries)
            {
               try
               {
                  Thread.sleep((long)(lDelay * (1.5 - RandUtil.getSecureRandom().nextFloat())));
               }
               catch (InterruptedException e)
               {
               }

               lDelay *= 2;

               if (lDelay > 10000)
               {
                  lDelay = 10000;
               }
            }
         }

         synchronized (this)
         {
            if (!m_bDispatcherStopped)
            {
               throw new ObjectQueueConnectionException("err.rpc.queueing.invokeRetries", new Object[]
               {
                  new Integer(nInvokeTries)
               }, lastThrowable);
            }
         }

         return null;
      }

      /**
       * Release a socket acquired from getSocket.
       * @param socket the socket to release.
       */
      protected synchronized void release(ObjectSocket socket)
      {
         if (m_socketSet != null)
         {
            try
            {
               socket.close();
            }
            catch (Throwable t)
            {
               s_logger.warn("Failed to close receiving socket", t);
            }

            m_socketSet.remove(socket);
            notifyAll();
         }
      }

      /**
       * Returns a socket bound to the dispatcher's address and port. May create
       * a new socket or recycle a socket previously released.
       * @return the socket.
       */
      protected synchronized ObjectSocket getSocket(boolean bWaitForDispatcher) throws InterruptedException
      {
         while (!m_bDispatcherKnown && !m_bPoolShutdown && bWaitForDispatcher)
         {
            wait();
         }

         if (m_bPoolShutdown || (!m_bDispatcherKnown && !bWaitForDispatcher))
         {
            return null;
         }

         try
         {
            ObjectSocket socket = createSocket();
            m_socketSet.add(socket);

            return socket;
         }
         catch (Throwable t)
         {
            if (!m_sDispatcherNodeName.equals(m_sNodeName))
            {
               invalidateDispatcher(); // check that dispatcher has not changed.
            }

            throw ObjUtil.rethrow(t);
         }
      }

      /**
       * Creates a socket connection to the dispatcher.
       * @return the socket.
       */
      protected ObjectSocket createSocket() throws Exception
      {
         ObjectSocket socket = new LocalObjectSocket();

         // Do not wait for socket to be accepted: createSocket is called from a
         // synchronized context, so blocking here
         // can lead to deadlock.
         ((LocalObjectServerSocket)m_serverSocket).write(socket, false);

         return socket;
      }

      /**
       * Get a new message.
       * @return SysDispatcherMessage The message returned.
       */
      protected DispatcherMessage getMessage()
      {
         s_logger.debug("Consumer pool received getMessage request");

         try
         {
            return (DispatcherMessage)invoke(DispatcherMessage.GET_MESSAGE, new Object[]
            {
               m_sNodeName,
               GET_MESSAGE_TIMEOUT
            }, false);
         }
         catch (Throwable t)
         {
            invalidateDispatcher();
         }

         return null;
      }

      /**
       * @see nexj.core.rpc.ra.GenericConsumerPool#listen()
       */
      protected void listen()
      {
         synchronized (ObjectConsumerPool.this)
         {
            synchronized (this)
            {
               if (m_bPoolShutdown != m_bShutdown)
               {
                  notifyAll();
               }

               m_bPoolShutdown = m_bShutdown;
            }
         }

         boolean bDispatcherKnown;

         synchronized (this)
         {
            bDispatcherKnown = m_bDispatcherKnown;
         }

         if (!bDispatcherKnown)
         {
            close();
            startDispatcher(); // initialize dispatcher

            synchronized (this)
            {
               if (!m_bDispatcherKnown)
               {
                  s_logger.debug("Failed to acquire dispatcher, retrying");

                  return;
               }

               getMessages(); // launch getMessages loop
            }
         }

         processMessages(); // handle requests to server
      }

      /**
       * Launch message pulling thread.
       */
      protected synchronized void getMessages()
      {
         m_messagePuller = new RepeatedWork("message-puller")
         {
            public void run()
            {
               boolean bEnabled = Logger.isEnabled();

               ObjectConsumer consumer = null;

               try
               {
                  ObjectQueueing.LOGGER.enable();

                  // get a message from the dispatcher (could block)
                  final DispatcherMessage message = getMessage();

                  // process the message from the dispatcher
                  if (message != null)
                  {
                     s_logger.debug("Receiving message");
                     consumer = (ObjectConsumer)getConsumer();
                     consumer.start(new DispatcherMessageDelegator()
                     {
                        public DispatcherMessage getMessage()
                        {
                           return message;
                        }

                        public void respond(Object result)
                        {
                           if (message.getType() == RECEIVE)
                           {
                              m_state.wake(new Long(System.currentTimeMillis()));
                           }
                        }

                        public void error(int nCause, Throwable t)
                        {
                           if (nCause == DISPATCHER_CHANGED)
                           {
                              invalidateDispatcher();
                           }

                           if (nCause == SHUTDOWN)
                           {
                              stop();
                           }
                        }
                     });

                     consumer = null;
                  }
               }
               catch (Throwable t)
               {
                  s_logger.error("Error in " + this, t);
               }
               finally
               {
                  if (consumer != null)
                  {
                     consumer.dispose();
                  }

                  Logger.setEnabled(bEnabled);
               }
            }
         };

         m_messagePuller.restart();
      }

      /**
       * Notify the pool that the dispatcher has changed.
       */
      protected void invalidateDispatcher()
      {
         m_bDispatcherKnown = false;
         wake(System.currentTimeMillis());
      }

      /**
       * Notify the pool of the dispatcher component state.
       * @param bStopped true if the dispatcher component has stopped.
       */
      protected synchronized void setStopped(boolean bStopped)
      {
         m_bDispatcherStopped = bStopped;
         m_bDispatcherKnown = m_bDispatcherKnown & bStopped;
      }

      /**
       * @return boolean true if the dispatcher component is stopped.
       */
      protected synchronized boolean isStopped()
      {
         return m_bDispatcherStopped;
      }

      /**
       * @return String the dispatcher node id.
       */
      protected synchronized String getDispatcherNodeName()
      {
         return m_sDispatcherNodeName;
      }

      /**
       * Revoke all nodes that have been specified by the ClusterManager since
       * the last call to revoke.
       */
      protected void revoke()
      {
      }

      /**
       * Remove a socket from the receiving set.
       * @param socket The socket to remove.
       */
      protected synchronized void removeSocket(ObjectSocket socket)
      {
         if (!m_bDistributed)
         {
            if (m_receivingSocketSet != null)
            {
               m_receivingSocketSet.remove(socket);
            }
         }
      }

      /**
       * Receive a request on a socket.
       * @param socket the socket on which to receive the request.
       */
      protected void receiveRequest(ObjectSocket socket)
      {
         if (!socket.isClosed())
         {
            String sDispatcherNodeName = null;

            synchronized (this)
            {
               if (m_bPoolShutdown)
               {
                  return;
               }

               if (m_receivingSocketSet == null)
               {
                  m_receivingSocketSet = new HashHolder();
               }

               m_receivingSocketSet.add(socket);
               sDispatcherNodeName = m_sDispatcherNodeName;
            }

            ObjectConsumer consumer = null;

            try
            {
               consumer = (ObjectConsumer)getConsumer();
               consumer.start(socket, sDispatcherNodeName, true);
               consumer = null;
            }
            catch (Throwable t)
            {
               s_logger.error("Failed to start consumer", t);
            }
            finally
            {
               if (consumer != null)
               {
                  consumer.dispose();
               }
            }
         }
      }
   }
}
