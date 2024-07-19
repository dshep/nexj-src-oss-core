package nexj.core.rpc.queueing;

import java.net.InetAddress;
import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import javax.naming.InitialContext;
import javax.resource.spi.ManagedConnectionFactory;
import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.TransactionManager;

import nexj.core.integration.ContextReceiver;
import nexj.core.integration.IntegrationException;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Primitive;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.queueing.ObjectDispatcherQueue;
import nexj.core.meta.integration.channel.queueing.ObjectQueue;
import nexj.core.monitoring.jmx.StatUtil;
import nexj.core.persistence.OID;
import nexj.core.persistence.Query;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.ServerException;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ActionContext;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.testing.unit.UnitTestPlayer;
import nexj.core.util.Binary;
import nexj.core.util.BinaryHeap;
import nexj.core.util.ComparableComparator;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Heap;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.auth.SimplePrincipal;

/**
 * Concurrency control component to support the ObjectQueueDispatcher class.
 */
public class ObjectQueueDispatcher extends ContextReceiver implements ObjectDispatchListener, Lifecycle
{
   // constants

   /**
    * The attributes to read off a queue for message receiving.
    */
   public final static Pair RECEIVE_ATTRIBUTES = Pair.list(Symbol.define("body"), Pair.list(Symbol.define("queue"), Symbol
      .define("timeout")));

   /**
    * The ONE symbol, indicating that a message is to be received by one
    * arbitrary node.
    */
   public final static Symbol ONE_RECEIVER = Symbol.define("one");

   /**
    * The ALL symbol, indicating that a message is to be received by all nodes.
    */
   public final static Symbol ALL_RECEIVERS = Symbol.define("all");

   /**
    * The DISPATCHER symbol, indicating that a message is to be received by the
    * dispatcher in sequence with dispatcher operations.
    */
   public final static Symbol DISPATCHER_RECEIVER = Symbol.define("dispatcher");

   /**
    * The OTHER symbol, indicating that a message is to be received by all nodes
    * except the sender.
    */
   public final static Symbol OTHER_RECEIVERS = Symbol.define("other");

   // attributes

   /**
    * The next dispatch time returned by the most recent call to dispatch.
    */
   protected Timestamp m_tsNextDispatchTime;

   /**
    * The name of the dispatcher's queue.
    */
   protected String m_sQueueName;

   /**
    * True if running inside a J2EE container.
    */
   protected boolean m_bContained = J2EEUtil.isContained();

   /**
    * True if running inside a JUnit J2EE container.
    */
   protected static boolean s_bTestContained;

   /**
    * True if the resource adapter has shutdown.
    */
   protected boolean m_bShutdown;

   /**
    * True if a dispatch request has been made and has not yet returned.
    */
   protected boolean m_bDispatch;

   /**
    * True if the component is enabled.
    */
   protected boolean m_bEnabled;

   /**
    * The most recent timestamp for which a message ordinal has been returned.
    */
   protected static Timestamp s_tsLastTimestamp;

   /**
    * The most recently returned message ordinal.
    */
   protected static int s_nOrdinal;

   /**
    * Maximum heap size.
    */
   protected int m_nMaxHeapSize = 100;

   // associations

   /**
    * Set of semaphores currently held on this node.
    */
   protected Lookup m_semaphoreMap = new HashTab();

   /**
    * Set of resources that have saturated semaphores.
    */
   protected Set m_blockingSet = new HashHolder();

   /**
    * Map of nodes by processing message Id.
    */
   protected Lookup m_nodeByProcessingMessageIdMap = new HashTab();

   /**
    * Map of sets of processing messages by Node.
    */
   protected Lookup m_processingMessagesByNodeNameMap = new HashTab();

   /**
    * Heap of messages deliverable to any node.
    */
   protected Heap m_deliverableAnyHeap = new BinaryHeap(ComparableComparator.INSTANCE);

   /**
    * Map of heaps of deliverable messages by node name.
    */
   protected Lookup m_deliverableHeapMap = new HashTab();

   /**
    * The attributes to load when reading a message.
    */
   protected Pair m_messageLoadAttributes;

   /**
    * The connection factory.
    */
   protected ObjectQueueConnectionFactory m_factory;

   /**
    * The metadata.
    */
   protected Metadata m_metadata;

   /**
    * The dispatcher channel stub.
    */
   protected ObjectDispatcherQueue m_channel;

   /**
    * Object for synchronous processing of messages for testing.
    */
   protected Object m_syncTestLock = new Object();

   /**
    * The receiver logger.
    */
   protected Logger m_logger;

   // operations

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      super.initialize();
      m_logger = Logger.getLogger(getClass());

      if (!m_bEnabled)
      {
         return;
      }

      if (m_bContained || s_bTestContained)
      {
         String sFactory = J2EEUtil.JNDI_ENV_PREFIX + "queueing/ObjectQueue";

         if (m_logger.isInfoEnabled())
         {
            m_logger.info("Binding to connection factory \"" + sFactory + "\"");
         }

         m_factory = (ObjectQueueConnectionFactory)new InitialContext().lookup(sFactory);
      }
      else
      {
         m_logger.info("Binding to ObjectQueue resource adapter connection factory");

         ManagedConnectionFactory mcf = (ManagedConnectionFactory)Class.forName(
            SysUtil.PACKAGE + ".core.rpc.queueing.ra.ObjectQueueManagedConnectionFactory").newInstance();

         m_factory = (ObjectQueueConnectionFactory)mcf.createConnectionFactory();
      }
   }

   /**
    * @return The dispatcher channel.
    */
   public ObjectDispatcherQueue getChannel()
   {
      return m_channel;
   }

   /**
    * The clientWake event.
    */
   public void clientWake()
   {
      boolean bEnabled = Logger.isEnabled();

      try
      {
         ObjectQueueing.LOGGER.enable();

         if (m_factory == null)
         {
            return;
         }

         m_logger.debug("Sending dispatcher \"wake\" request");

         ObjectQueueConnection con = null;

         try
         {
            con = m_factory.open();
            con.wake();
         }
         catch (IntegrationException e)
         {
            throw e;
         }
         catch (Exception e)
         {
            throw new RPCException("err.rpc.msg", e);
         }
         finally
         {
            if (con != null)
            {
               con.close();
            }
         }
      }
      finally
      {
         Logger.setEnabled(bEnabled);
      }
   }

   /**
    * @return The name of the current node.
    */
   protected String getNodeName()
   {
      return J2EEUtil.ISOLATED_NODE_NAME;
   }

   /**
    * Set maximum heap size.
    * @param maxHeapSize Maximum heap size.
    */
   public void setMaxHeapSize(int nMaxHeapSize)
   {
      m_nMaxHeapSize = nMaxHeapSize;
   }

   /**
    * @return Maximum heap size.
    */
   public int getMaxHeapSize()
   {
      return m_nMaxHeapSize;
   }

   /**
    * The clientPost event. Posts a command for delivery to one or several
    * nodes.
    * 
    * @param tobj The serializable message to post.
    */
   public void clientPost(TransferObject tobj)
   {
      if (m_factory == null)
      {
         return;
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Sending dispatcher \"post\" request: " + tobj);
      }

      ObjectQueueConnection con = null;

      try
      {
         con = m_factory.open();
         con.post(tobj);
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new RPCException("err.rpc.msg", e);
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }
      }
   }

   /**
    * Returns the heap associated with sNode, creating one if necessary.
    * 
    * @param sNodeName The node for which to return a heap.
    * @return Heap The heap.
    */
   protected Heap getHeap(String sNodeName)
   {
      Heap heap = (Heap)m_deliverableHeapMap.get(sNodeName);

      if (heap == null)
      {
         if (isAvailable(sNodeName))
         {
            heap = new BinaryHeap(ComparableComparator.INSTANCE);
            m_deliverableHeapMap.put(sNodeName, heap);
         }
         else
         {
            return null;
         }
      }

      return heap;
   }

   /**
    * Pushes a message for delivery.
    * 
    * @param nPriority The priority of the message.
    * @param nOrdinal The sequential position of the message.
    * @param sNodeName The node defining the distribution.
    * @param nDistribution The distribution category, one of the constants from
    *           ObjectDispatchListner: ANY - message must be processed by any
    *           one node. ALL - message must be processed by all nodes. ONLY -
    *           message must be processed by sNode. EXCEPT - message must be
    *           processed by all nodes except sNode. DISPATCHER - message must
    *           be processed synchronously by the dispatcher node.
    * @param message The message to deliver.
    */
   protected void pushMessage(int nPriority, int nOrdinal, String sNodeName, int nDistribution, DispatcherMessage message)
   {
      Delivery delivery = new Delivery(nPriority, nOrdinal, message, (nDistribution == ObjectDispatchListener.DISPATCHER));

      switch (nDistribution)
      {
         case ObjectDispatchListener.ANY:
            addDelivery(m_deliverableAnyHeap, "Any", delivery);

            break;
         case ObjectDispatchListener.DISPATCHER:
         case ObjectDispatchListener.ONLY:
         {
            Heap nodeHeap = getHeap(sNodeName);

            if (nodeHeap == null)
            {
               throw new UncheckedException("err.queueing.unknownNode", new Object[]
               {
                  sNodeName
               });
            }
            else
            {
               addDelivery(nodeHeap, sNodeName, delivery);
            }

            break;
         }
         case ObjectDispatchListener.ALL:
         case ObjectDispatchListener.EXCEPT:
            HeapFullException exc = null;
            boolean bNotifyDispatcher = false;

            for (Lookup.Iterator iter = m_deliverableHeapMap.iterator(); iter.hasNext();)
            {
               String sCurrNode = (String)iter.next();

               if (nDistribution == ObjectDispatchListener.ALL || !sCurrNode.equals(sNodeName))
               {
                  try
                  {
                     addDelivery((Heap)iter.getValue(), sCurrNode, delivery);
                     bNotifyDispatcher = true;
                  }
                  catch (HeapFullException e)
                  {
                     exc = e;
                  }
               }
            }

            if (exc != null)
            {
               exc.setNotifyDispatcher(bNotifyDispatcher);

               throw exc;
            }

            break;
         default:
            throw new UncheckedException("err.queueing.invalidDistribution", new Object[]
            {
               Primitive.createInteger(nDistribution)
            });
      }
   }

   /**
    * Add delivery to the heap. This will throw HeapFullException if the heap is full.
    * @param heap Heap.
    * @param sHeapName Heap name.
    * @param delivery Delivery.
    */
   private void addDelivery(Heap heap, String sHeapName, Delivery delivery)
   {
      if (heap.size() > m_nMaxHeapSize)
      {
         throw new HeapFullException(sHeapName);
      }

      heap.add(delivery);
   }

   /**
    * Determines if a node is available as the destination for delivery.
    * 
    * @param sNode The node.
    * @return true if the node is available.
    */
   public boolean isAvailable(String sNodeName)
   {
      return sNodeName.equals(J2EEUtil.ISOLATED_NODE_NAME);
   }

   /**
    * Pulls the next message for delivery.
    * 
    * @param sNodeName The node for which to pull a message.
    * @return Delivery The delivery, null if none are available.
    */
   protected Delivery pullMessage(String sNodeName)
   {
      if (!isAvailable(sNodeName))
      {
         m_deliverableHeapMap.remove(sNodeName);

         return new Delivery();
      }

      Delivery anyDelivery = (Delivery)m_deliverableAnyHeap.first();
      Delivery nodeDelivery = (Delivery)getHeap(sNodeName).first();
      Delivery delivery = anyDelivery;

      if (delivery == null || nodeDelivery != null && delivery.compareTo(nodeDelivery) > 0)
      {
         delivery = nodeDelivery;
      }

      if (delivery != null)
      {
         if (delivery == anyDelivery)
         {
            m_deliverableAnyHeap.removeFirst();
         }
         else
         {
            getHeap(sNodeName).removeFirst();
         }
      }

      return delivery;
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectDispatchListener#onMessage(nexj.core.rpc.queueing.DispatcherMessage)
    */
   public void onMessage(DispatcherMessage message)
   {
      try
      {
         message.respond(onMessage(message.getType(), message.getArgArray(), message.getDispatcherName()));
      }
      catch (ShutdownException e)
      {
         message.error(DispatcherMessage.SHUTDOWN, e);
      }
      catch (InvalidDispatcherException e)
      {
         message.error(DispatcherMessage.DISPATCHER_CHANGED, e);
      }
      catch (ServerException e)
      {
         if (e.getCause() instanceof InvalidDispatcherException)
         {
            message.error(DispatcherMessage.DISPATCHER_CHANGED, e);
         }
         else
         {
            message.error(DispatcherMessage.EXCEPTION, e);
            ObjUtil.rethrow(e);
         }
      }
      catch (Throwable t)
      {
         message.error(DispatcherMessage.EXCEPTION, t);
         ObjUtil.rethrow(t);
      }
   }

   /**
    * @see nexj.core.rpc.queueing.ObjectDispatchListener#onMessage(int,
    *      java.lang.Object[])
    */
   protected Object onMessage(int nMessageType, Object[] args, String sDispatcherId)
   {
      switch (nMessageType)
      {
         // dispatcher node operations
         case DispatcherMessage.START_NODE:
            return startNode((InetAddress)args[0]);

         case DispatcherMessage.DISPATCH:
            return dispatch(sDispatcherId);

         case DispatcherMessage.GET_MESSAGE:
            return getMessage((String)args[0], ((Number)args[1]).longValue(), sDispatcherId);

         case DispatcherMessage.POST: // post an non-persisted serializable message
            post((TransferObject)args[0], sDispatcherId);

            return null;

         case DispatcherMessage.REVOKE: // handle node unavailability
            revoke((List)args[0], (String)args[1]);

            return null;

            // client node operations
         case DispatcherMessage.RECEIVE: // receive a persisted message
            receive((Binary)args[0], (String)args[1], sDispatcherId);

            return null;

         case DispatcherMessage.EXECUTE: // receive an non-persisted message
            execute((TransferObject)args[0]);

            return null;

         default:
            throw new UncheckedException("err.queueing.invalidMethod", new Object[]
            {
               Primitive.createInteger(nMessageType)
            });
      }
   }

   /**
    * Notifies the dispatcher that a cluster node is no longer available.
    * 
    * @param unavailableNodeList The list of unavailable nodes.
    * @param sDispatcherId The name of the dispatcher.
    */
   public void revoke(List unavailableNodeList, final String sDispatcherId)
   {
   }

   /**
    * Posts a command for delivery to one or several nodes.
    * 
    * @param tobj The serializable message to post.
    */
   protected synchronized void post(final TransferObject tobj, final String sDispatcherId)
   {
      int nDistribution;
      String sHTTPNode = null;
      Object receiver = tobj.getValue(ObjectSender.RECEIVER);

      if (ONE_RECEIVER.equals(receiver))
      {
         nDistribution = ANY;
      }
      else if (ALL_RECEIVERS.equals(receiver))
      {
         nDistribution = ALL;
      }
      else if (DISPATCHER_RECEIVER.equals(receiver))
      {
         nDistribution = DISPATCHER;
      }
      else if (OTHER_RECEIVERS.equals(receiver))
      {
         nDistribution = EXCEPT;
         sHTTPNode = getNodeName();
      }
      else
      {
         nDistribution = ONLY;
         sHTTPNode = (String)receiver;
      }

      try
      {
         pushMessage(((Number)tobj.getValue(ObjectSender.PRIORITY)).intValue(), 0,
            (nDistribution == DISPATCHER) ? sDispatcherId : sHTTPNode, nDistribution,
            new DispatcherMessage()
            {
               public int getType()
               {
                  return EXECUTE;
               }

               public Object[] getArgArray()
               {
                  return new Object[]
                  {
                     tobj
                  };
               }

               public void respond(Object result)
               {
               }

               public void error(int nCause, Throwable t)
               {
               }

               public String getDispatcherName()
               {
                  return sDispatcherId;
               }
            });
      }
      catch (HeapFullException e)
      {
         if (e.isNotifyDispatcher())
         {
            // Not all heaps were full.
            notifyAll();
         }

         ObjUtil.rethrow(e);
      }

      notifyAll();
   }

   /**
    * If this node is the dispatcher, initialize the dispatcher. Else return the
    * address of the dispatcher node.
    * 
    * @param previousAddress The network address of to which we previously connected, or null.
    * @return Object[] The array of cluster attributes, indexed by
    *         ObjectDispatchListener constants. Null unless running in
    *         distributed mode.
    */
   protected synchronized Object startNode(final InetAddress previousAddress)
   {
      m_logger.debug("Initializing ObjectQueue dispatcher");

      final Object[] result = new Object[1];

      m_deliverableHeapMap.clear();
      m_deliverableAnyHeap.clear();

      run(new ContextRunnable()
      {
         public void err(Throwable t, InvocationContext context) throws Throwable
         {
            throw new UncheckedException("err.queueing.startup", t);
         }

         public String getClientAddress() throws Throwable
         {
            return "dispatcher";
         }

         public String getUser() throws Throwable
         {
            return null;
         }

         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         public void run(InvocationContext context) throws Throwable
         {
            context.setSecure(false);

            Metaclass dispatcherClass = m_metadata.getMetaclass("SysObjectQueueDispatcher");
            Instance dispatcher = (Instance)dispatcherClass.invoke("getDispatcher", new Object[]
            {
               Boolean.TRUE
            });

            m_sQueueName = (String)dispatcher.getValue("QUEUE_NAME");
            m_semaphoreMap = new HashTab();
            m_blockingSet = new HashHolder();
            m_nodeByProcessingMessageIdMap = new HashTab();
            m_processingMessagesByNodeNameMap = new HashTab();
            m_messageLoadAttributes = (Pair)dispatcher.getValue("SELECT_ATTRIBUTES", "LOAD_ATTRIBUTES");

            if (startNode(result, dispatcher, previousAddress, context))
            {
               m_logger.debug("This node selected to be dispatcher, recovering incomplete transactions");
               dispatcherClass.invoke("recover", new Object[]
               {
                  null
               });
            }
         }
      }, m_channel, "ObjectQueue");

      m_logger.dump("StartNode notifying all threads");
      notifyAll();

      if (result[0] == null)
      {
         return null;
      }

      return result[0];
   }

   /**
    * Initialize connection to the application cluster.
    * 
    * @param startResult Result value from the call to dispatcher.startNode().
    * @param dispatcher The dispatcher.
    * @param previousAddress The network address to which we previously connected, or null.
    * @param context The invocation context.
    * @return True if this node is the dispatcher.
    */
   protected boolean startNode(Object[] startResult, Instance dispatcher, InetAddress previousAddress, InvocationContext context)
   {
      getHeap(J2EEUtil.ISOLATED_NODE_NAME);

      return true;
   }

   /**
    * Returns the message Id of a message cleared for delivery. If timeout
    * expires before a message becomes available, returns null.
    * 
    * @param sCallingNode The name of the node requesting a message, may be null
    *           in a non-clustered environment.
    * @param lTimeout The timeout in milliseconds.
    * @return SysDispatcherMessage The message to be delivered to the calling
    *         node.
    */
   protected synchronized DispatcherMessage getMessage(final String sCallingNode, long lTimeout, final String sDispatcherId)
   {
      if (m_bShutdown)
      {
         throw new ShutdownException();
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Received getMessage request from node " + sCallingNode);
      }

      Delivery delivery = null;

      try
      {
         long lStartTime = System.currentTimeMillis();
         delivery = pullMessage(sCallingNode);

         // wait for deliverable messages
         while (delivery == null && !m_bShutdown)
         {
            if (m_bDispatch && m_tsNextDispatchTime == null)
            {
               // call to dispatch has been requested and has not yet been handled.
               run(new ContextRunnable()
               {
                  private Timestamp handleContext(InvocationContext context, Boolean safety)
                  {
                     context.setSecure(false);

                     Metaclass dispatcherClass = m_metadata.getMetaclass("SysObjectQueueDispatcher");

                     // verify that dispatcher remains the same
                     if (!sDispatcherId.equals(J2EEUtil.ISOLATED_NODE_NAME) &&
                        !sDispatcherId.equals(((Instance)((Instance)dispatcherClass.invoke("getDispatcher", new Object[]{Boolean.FALSE})).getValue("node")).getValue("name")))
                     {
                        throw new InvalidDispatcherException(sDispatcherId);
                     }

                     // dispatch in bulk
                     Pair result = (Pair)dispatcherClass.invoke("dispatch", new Object[]{safety});
                     InstanceList deliverable = (InstanceList)result.getTail();

                     if (deliverable != null && !deliverable.isEmpty())
                     {
                        for (int i = 0; i < deliverable.size(); i++)
                        {
                           final Instance message = deliverable.getInstance(i);
                           final Instance queue = (Instance)message.getValue("queue");
                           final String sQueueName = (String)queue.getValue("name");
                           Number priority = (Number)queue.getValue("priority");

                           pushMessage((priority == null) ? 1000 : priority.intValue(), i, sDispatcherId, (sQueueName
                              .equals(m_sQueueName)) ? ObjectDispatchListener.DISPATCHER : ObjectDispatchListener.ANY,
                              new DispatcherMessage()
                              {
                                 public int getType()
                                 {
                                    return RECEIVE;
                                 }

                                 public Object[] getArgArray()
                                 {
                                    return new Object[]
                                    {
                                       message.getOID().toBinary(),
                                       sQueueName
                                    };
                                 }

                                 public void error(int nCause, Throwable t)
                                 {
                                 }

                                 public void respond(Object result)
                                 {
                                 }

                                 public String getDispatcherName()
                                 {
                                    return sDispatcherId;
                                 }
                              });
                        }
                     }

                     if (m_logger.isDebugEnabled())
                     {
                        m_logger.debug("Dispatcher returned with " + ((deliverable == null) ? 0 : deliverable.size())
                           + " messages available for delivery");

                        if (m_logger.isDumpEnabled())
                        {
                           m_logger.dump("Dispatcher will run again at " + result.getHead());
                        }
                     }

                     m_logger.dump("GetMessage notifying all threads");
                     ObjectQueueDispatcher.this.notifyAll(); // alert calling
                                                             // nodes that new
                                                             // messages may be
                                                             // available.

                     return (Timestamp)result.getHead();
                  }

                  public void run(InvocationContext context) throws Throwable
                  {
                     m_tsNextDispatchTime = handleContext(context, Boolean.FALSE);
                  }

                  public boolean isEnabled() throws Throwable
                  {
                     return true;
                  }

                  public String getUser() throws Throwable
                  {
                     return null;
                  }

                  public String getClientAddress() throws Throwable
                  {
                     return "dispatcher";
                  }

                  public void err(Throwable t, InvocationContext context) throws Throwable
                  {
                     if (t instanceof InvalidDispatcherException)
                     {
                        throw t;
                     }

                     m_logger.debug("Dispatching in bulk failed, retrying with one transaction per operation", t);

                     context.initUnitOfWork();

                     try
                     {
                        m_tsNextDispatchTime = handleContext(context, Boolean.TRUE);

                        context.complete(true);
                     }
                     catch (Throwable tt)
                     {
                        context.complete(false);

                        throw tt;
                     }
                  }
               }, m_channel, "ObjectQueue");
            }

            delivery = pullMessage(sCallingNode);

            if (delivery == null)
            {
               if (lTimeout <= 0)
               {
                  break;
               }

               wait(lTimeout);
               m_logger.dump("GetMessage returned from wait");
               lTimeout = lTimeout + (lStartTime - System.currentTimeMillis());

               delivery = pullMessage(sCallingNode);
            }
         }

         if (m_bShutdown)
         {
            throw new ShutdownException();
         }

         if (delivery == null)
         {
            if (m_logger.isDumpEnabled())
            {
               m_logger.dump("No message ready within timeout, getMessage returning to node " + sCallingNode);
            }

            return null;
         }
      }
      catch (InterruptedException e)
      {
         if (m_logger.isDumpEnabled())
         {
            m_logger.dump("GetMessage interrupted, returning to node " + sCallingNode);
         }

         return null;
      }

      if (delivery.isUnavailable())
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("GetMessage returning because node " + sCallingNode + " is unavailable");
         }
      }
      else
      {
         if (delivery.m_bSynchronous)
         {
            // deliver in sequence with other dispatcher operations
            onMessage(delivery.m_message);
         }
         else
         {
            delivery.deliver(sCallingNode);

            return delivery.m_message;
         }
      }

      return null;
   }

   /**
    * Update the deliverable list and notify all waiting threads. Returns the
    * timestamp of the next anticipated change to the list.
    * 
    * @return The timestamp of the next anticipated change to the list.
    */
   protected synchronized Timestamp dispatch(String sDispatcherId)
   {
      m_logger.debug("Dispatching");

      Timestamp nextDispatchTime = null;

      if (m_bDispatch)
      {
         // A dispatch request is already in progress. Return immediately,
         // leaving the original
         // request to pick up the next dispatch response.
         if (m_tsNextDispatchTime != null)
         {
            // the in-progress request is awake, but hasn't picked up the
            // response yet.
            nextDispatchTime = m_tsNextDispatchTime;
            // when the in-progress request sees null, it will re-notify all
            // waiting threads.
            m_tsNextDispatchTime = null;
         }
      }
      else
      {
         m_bDispatch = true;
         assert (m_tsNextDispatchTime == null);

         try
         {
            while (m_tsNextDispatchTime == null && !m_bShutdown)
            {
               m_logger.dump("Dispatch notifying all threads");
               notifyAll();
               wait();
               m_logger.dump("Dispatch returned from wait");
            }

            nextDispatchTime = m_tsNextDispatchTime;
            m_tsNextDispatchTime = null;
         }
         catch (InterruptedException e)
         {
            return null;
         }
         finally
         {
            m_bDispatch = false;
         }
      }

      if (m_bShutdown)
      {
         throw new ShutdownException();
      }

      return nextDispatchTime;
   }

   /**
    * Receive a message. Helper for receive and getMessage.
    * 
    * @param context The context.
    * @param sQueueName The name of the queue.
    * @param channel The channel associated with the queue.
    * @param msg The messsage.
    * @param dispatcherClass The dispatcher class.
    */
   protected void receiveRun(InvocationContext context, String sQueueName, ObjectQueue channel, Instance msg,
      Metaclass dispatcherClass, String sDispatcherId)
   {
      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Receiving message with oid " + msg.getOID() + " from queue " + sQueueName
            + ((channel == null) ? "" : (" (" + channel.getName() + ")")) + " as user \"" + msg.getValue("user") + "\"");
      }

      // login the message sender
      context.login(new SimplePrincipal((String)msg.getValue("user")));

      // check that message has not been recovered by the dispatcher
      if (!((Boolean)msg.findValue("isProcessing", Boolean.TRUE)).booleanValue())
      {
         throw new InvalidDispatcherException(sDispatcherId);
      }

      // set unit of work timeout
      Number timeout = ((Number)((Instance)msg.getValue("queue")).getValue("timeout"));
      long lUnitOfWorkTimeout = (timeout == null) ? 0 : timeout.longValue();
      TransactionManager tm = (TransactionManager)m_metadata.getComponent("System.TransactionManager").getInstance(context);

      context.getUnitOfWork().setTimeout(lUnitOfWorkTimeout);

      try
      {
         TransferObject body = (TransferObject)msg.getValue("body");

         tm.setTransactionTimeout(86400); // 24 hours

         if (channel == null || body == null ||
            !((ObjectReceiver)channel.getReceiver().getInstance(context))
               .receiveServerMessage(context, msg, body))
         {
            m_logger.dump("Invoking receive");

            msg.invoke("receive");
         }
      }
      catch (SystemException e)
      {
         ObjUtil.rethrow(e);
      }

      if (!((Boolean)msg.getValue("commitRequired")).booleanValue())
      {
         // rollback everything except the message instance
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Message marked for rollback, rolling back:" + msg);
         }

         TransferObject detachedTobj = (TransferObject)RPCUtil.transfer(msg, null, RPCUtil.TF_ALL | RPCUtil.TF_DIRTY);

         context.complete(false);
         context.initUnitOfWork();

         // check that message has not been recovered by the dispatcher
         if (!((Boolean)msg.findValue("isProcessing", Boolean.TRUE)).booleanValue())
         {
            throw new InvalidDispatcherException(sDispatcherId);
         }

         RPCUtil.instantiate(detachedTobj, context, false);
      }

      boolean bSecured = context.isSecure();

      try
      {
         context.setSecure(false);
         dispatcherClass.invoke("clientComplete", new Object[] { msg });
      }
      finally
      {
         context.setSecure(bSecured);
      }
   }

   /**
    * Handles receive failure. Helper for receive and getMessage.
    * 
    * @param context The invocation context.
    * @param t The failure.
    * @param oid The oid of the failed message.
    * @param sDispatcherId The id of the message's dispatcher.
    */
   protected void receiveErr(InvocationContext context, Throwable t, OID oid, String sDispatcherId) throws Throwable
   {
      if (t instanceof InvalidDispatcherException)
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Dispatcher changed during delivery of message with oid " + oid, t);
         }

         throw t;
      }

      if (m_logger.isDebugEnabled())
      {
         m_logger.debug("Delivery failure for message with oid " + oid + ", calling fail", t);
      }

      Metaclass dispatcherClass = m_metadata.getMetaclass("SysObjectQueueDispatcher");
      Instance msg = readMessage(oid, m_messageLoadAttributes, context);

      if (msg == null)
      {
         return; // message has been canceled. Unlikely, but possible.
      }

      // login the message sender
      context.login(new SimplePrincipal((String)msg.getValue("user")));

      // check that message has not been recovered by the dispatcher
      if (!((Boolean)msg.findValue("isProcessing", Boolean.TRUE)).booleanValue())
      {
         throw new InvalidDispatcherException(sDispatcherId);
      }

      // invoke fail method
      msg.invoke("fail", new Object[] {t});

      boolean bSecured = context.isSecure();

      try
      {
         context.setSecure(false);
         dispatcherClass.invoke("clientComplete", new Object[] {msg});
      }
      finally
      {
         context.setSecure(bSecured);
      }
   }

   /**
    * Blacklist a message.
    * 
    * @param sDispatcherId The id of the dispatcher at the time the message was
    *           marked for processing
    * @param context The invocation context.
    * @param oid The oid of the message to blacklist.
    * @param t The exception leading to the blacklist.
    */
   protected void blacklist(String sDispatcherId, InvocationContext context, OID oid, Throwable t) throws Throwable
   {
      if (m_logger.isWarnEnabled())
      {
         m_logger.warn("Delivery failed for message with oid " + oid + ", blacklisting message", t);
      }

      context.initUnitOfWork();

      boolean bSecured = context.isSecure();

      try
      {
         Metaclass dispatcherClass = m_metadata.getMetaclass("SysObjectQueueDispatcher");
         InstanceList messages = Query.createRead(m_metadata.getMetaclass("SysMessage"), m_messageLoadAttributes, Pair.attribute("").eq(oid),
            null, -1, 0, false, Query.SEC_NONE, context).read();

         Instance msg = (messages == null || messages.isEmpty()) ? null : messages.getInstance(0);

         if (msg == null)
         {
            context.complete(false);

            return; // message doesn't exist, can't blacklist it.
         }

         // check that message has not been recovered by the dispatcher
         if (!((Boolean)msg.findValue("isProcessing", Boolean.TRUE)).booleanValue())
         {
            throw new InvalidDispatcherException(sDispatcherId);
         }

         context.setSecure(false);

         // mark message as undeliverable.
         dispatcherClass.invoke("blacklist", new Object[]
         {
            msg
         });

         context.complete(true);
      }
      catch (Throwable tt)
      {
         context.complete(false);

         throw tt;
      }
      finally
      {
         context.setSecure(bSecured);
      }
   }

   /**
    * Receives and a non-persisted message on the client node.
    * 
    * @param tobj The message.
    */
   protected void execute(final TransferObject tobj)
   {
      final long lStartTime = System.nanoTime();
      final String sQueueName = (String)tobj.getValue(ObjectSender.CHANNEL);
      final Channel channel = m_metadata.getChannel(sQueueName);

      run(new ContextRunnable()
      {
         public void err(Throwable t, InvocationContext context) throws Throwable
         {
            m_logger.error("Failed to receive non-persisted message " + tobj, t);
         }

         public String getClientAddress() throws Throwable
         {
            return sQueueName;
         }

         public String getUser() throws Throwable
         {
            return (String)tobj.getValue(ObjectSender.USER);
         }

         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         public void run(InvocationContext context) throws Throwable
         {
            try
            {
               // set unit of work timeout
               Number timeout = ((Number)((Instance)m_metadata.getMetaclass("SysQueue")
                  .invoke("getQueue", new Object[] {sQueueName})).getValue("timeout"));
               long lUnitOfWorkTimeout = (timeout == null) ? 0 : timeout.longValue();
               TransactionManager tm = (TransactionManager)m_metadata.getComponent("System.TransactionManager").getInstance(context);

               context.getUnitOfWork().setTimeout(lUnitOfWorkTimeout);

               try
               {
                  tm.setTransactionTimeout(86400); // 24 hours
                  ((ObjectReceiver)channel.getReceiver().getInstance(context)).receiveServerMessage(context, null, tobj);
               }
               catch (SystemException e)
               {
                  ObjUtil.rethrow(e);
               }
            }
            finally
            {
               StatUtil.updateAverage(context, "Object Queue/" + sQueueName,
                     Channel.STAT_AVERAGE_RECEIVE_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
            }
         }
      }, channel, "ObjectQueue");
   }

   /**
    * Reads a message instance.
    * 
    * @param oid The id of the message.
    * @param attributes The message attributes to load.
    * @param context The invocation context.
    * @return The message, null if not found.
    */
   protected Instance readMessage(OID oid, Pair attributes, InvocationContext context)
   {
      InstanceList messages = Query.createRead(m_metadata.getMetaclass("SysMessage"), attributes, Pair.attribute("").eq(oid),
         null, -1, 0, false, Query.SEC_NONE, context).read();

      Instance message = (messages == null || messages.isEmpty()) ? null : messages.getInstance(0);

      if (message != null)
      {
         boolean bWasSecured = context.isSecure();

         context.setSecure(false);

         try
         {
            message.setValue("updateEnabled", Boolean.TRUE);
         }
         finally
         {
            context.setSecure(bWasSecured);
         }
      }

      return message;
   }

   /**
    * Receives a message on a client node.
    * 
    * @param id The id of the message to receive.
    * @param sQueueName The name of the queue from which the message is
    *           delivered.
    */
   protected void receive(final Binary id, final String sQueueName, final String sDispatcherId)
   {
      final long lStartTime = System.nanoTime();
      Channel candidateChannel = m_metadata.findChannel(sQueueName);
      final ObjectQueue channel = (candidateChannel instanceof ObjectQueue) ? (ObjectQueue)candidateChannel : null;
      final OID oid = OID.fromBinary(id);

      run(new ContextRunnable()
      {
         public void run(InvocationContext context) throws Throwable
         {
            try
            {
               Instance msg = readMessage(oid, Pair.append(RECEIVE_ATTRIBUTES, m_messageLoadAttributes), context);

               if (msg == null)
               {
                  throw new UncheckedException("err.queueing.receive", new Object[]
                  {
                     oid
                  });
               }

               Metaclass dispatcherClass = m_metadata.getMetaclass("SysObjectQueueDispatcher");
               Channel candidateChannel = m_metadata.findChannel(sQueueName);
               final ObjectQueue channel = (candidateChannel instanceof ObjectQueue) ? (ObjectQueue)candidateChannel : null;

               try
               {
                  receiveRun(context, sQueueName, channel, msg, dispatcherClass, sDispatcherId);
               }
               catch (Throwable t)
               {
                  context.complete(false);
                  context.initUnitOfWork();
                  receiveErr(context, t, oid, sDispatcherId);
               }
            }
            finally
            {
               StatUtil.updateAverage(context, "Object Queue/" + sQueueName,
                     Channel.STAT_AVERAGE_RECEIVE_TIME, (double)(System.nanoTime() - lStartTime) / 1000000);
            }
         }

         public boolean isEnabled() throws Throwable
         {
            return true;
         }

         public String getUser() throws Throwable
         {
            return null;
         }

         public String getClientAddress() throws Throwable
         {
            return sQueueName;
         }

         public void err(Throwable t, InvocationContext context) throws Throwable
         {
            if (!(t instanceof InvalidDispatcherException))
            {
               blacklist(sDispatcherId, context, oid, t);
            }
         }
      }, (channel == null) ? m_channel : channel, "ObjectQueue");
   }

   /**
    * Acquire a semaphore. Note: not thread-safe. Assumes caller is the single
    * dispatcher thread.
    * 
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    * @return True if the semaphore is acquired, false otherwise.
    */
   public static boolean acquireSemaphore(Metaclass metaclass, ObjectQueueDispatcher comp, Pair semaphoreDef, ActionContext actx)
   {
      return comp.acquire(semaphoreDef);
   }

   /**
    * @deprecated Acquire a semaphore. Note: not thread-safe. Assumes caller is
    *             the single dispatcher thread.
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    * @return True if the semaphore is acquired, false otherwise.
    */
   public static Boolean acquireSemaphore(Metaclass metaclass, InvocationContext context, ObjectQueueDispatcher comp,
      Pair semaphoreDef, ActionContext actx)
   {
      return Boolean.valueOf(acquireSemaphore(metaclass, comp, semaphoreDef, actx));
   }

   /**
    * Release a semaphore on commit. Note: not thread-safe. Assumes caller is
    * the single dispatcher thread.
    * 
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    */
   public static void releaseSemaphore(Metaclass metaclass, ObjectQueueDispatcher comp, Pair semaphoreDef, ActionContext actx)
   {
      comp.release(semaphoreDef);
   }

   /**
    * @ deprecated Release a semaphore on commit. Note: not thread-safe. Assumes
    * caller is the single dispatcher thread.
    * 
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    */
   public static void releaseSemaphore(Metaclass metaclass, InvocationContext context, ObjectQueueDispatcher comp,
      Pair semaphoreDef, ActionContext actx)
   {
      releaseSemaphore(metaclass, comp, semaphoreDef, actx);
   }

   /**
    * Increments a semaphore.
    * 
    * @param resource The resource for which to increment the semaphore.
    * @param maxCount The maximum count for the resource. null leave maximum
    *           unchanged, and to ignore the maximum for this operation only.
    * @return true if the semaphore is acquired, false otherwise.
    */
   private boolean incrementSemaphore(Binary resource, Number maxCount)
   {
      Semaphore semaphore = (Semaphore)m_semaphoreMap.get(resource);

      if (semaphore == null)
      {
         semaphore = new Semaphore();
         m_semaphoreMap.put(resource, semaphore);
      }

      boolean bAcquired = semaphore.acquire(maxCount);

      if (m_logger.isDumpEnabled())
      {
         m_logger.dump((bAcquired) ? "Acquired" : "Not acquired");
      }

      if (bAcquired && semaphore.isSaturated())
      {
         m_blockingSet.add(semaphore);
      }

      return bAcquired;
   }

   /**
    * Acquire a semaphore. Note: not thread-safe. Assumes caller is the single
    * dispatcher thread.
    * 
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    * @return True if the semaphore is acquired, false otherwise.
    */
   protected boolean acquire(Pair semaphoreDef)
   {
      if (m_logger.isDumpEnabled())
      {
         m_logger.dump("Acquiring semaphore " + semaphoreDef);
      }

      final Binary resource = (Binary)semaphoreDef.getHead();
      boolean bAcquired = incrementSemaphore(resource, (Number)semaphoreDef.getTail());

      if (m_logger.isDumpEnabled())
      {
         m_logger.dump((bAcquired) ? "Acquired" : "Not acquired");
      }

      if (bAcquired)
      {
         Function compensator = new Function()
         {
            public boolean invoke(int nArgCount, Machine machine)
            {
               int nTxState = ((Number)machine.getArg(nArgCount - 1, nArgCount)).intValue();

               if (nTxState == Status.STATUS_ROLLEDBACK)
               {
                  release(resource);
               }

               return false;
            }
         };

         // if transaction rolls back, rollback the acquisition.
         ((InvocationContext)ThreadContextHolder.getContext()).getUnitOfWork().addCompensator(compensator, compensator);
      }

      return bAcquired;
   }

   /**
    * Release the semaphore protecting resource.
    * 
    * @param resource The resource of the semaphore to release.
    */
   private void release(Binary resource)
   {
      Semaphore semaphore = (Semaphore)m_semaphoreMap.get(resource);

      if (semaphore != null)
      {
         if (m_logger.isDumpEnabled())
         {
            m_logger.dump("Releasing semaphore " + resource);
         }

         if (semaphore.release())
         {
            m_semaphoreMap.remove(resource);
         }

         if (!semaphore.isSaturated())
         {
            m_blockingSet.remove(semaphore);
         }
      }
   }

   /**
    * Release a semaphore on commit. Note: not thread-safe. Assumes caller is
    * the single dispatcher thread.
    * 
    * @param semaphoreDef The (resource . maxCount) pair defining the semaphore.
    */
   protected void release(Pair semaphoreDef)
   {
      final Binary resource = (Binary)semaphoreDef.getHead();

      release(resource); // release the resource immediately

      Function compensator = new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            int nTxState = ((Number)machine.getArg(nArgCount - 1, nArgCount)).intValue();

            if (nTxState == Status.STATUS_ROLLEDBACK)
            {
               // release will be called for the resource again in a future
               // transaction, so we need to reset the resource count.
               incrementSemaphore(resource, null);
            }

            return false;
         }
      };

      ((InvocationContext)ThreadContextHolder.getContext()).getUnitOfWork().addCompensator(compensator, compensator);
   }

   /**
    * Complete a message delivery.
    * 
    * @param oid The oid of the message that has been delivered.
    */
   public static void complete(Metaclass metaclass, final ObjectQueueDispatcher comp, final OID oid, ActionContext actx)
   {
      Function compensator = new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            int nTxState = ((Number)machine.getArg(nArgCount - 1, nArgCount)).intValue();

            if (nTxState == Status.STATUS_COMMITTED)
            {
               comp.complete(oid);
            }

            return false;
         }
      };

      ((InvocationContext)ThreadContextHolder.getContext()).getUnitOfWork().addCompensator(compensator, compensator);
   }

   /**
    * @param deliveryTime The delivery time for which
    * @return Message ordinal that is larger than all previous ordinals for the
    *         given deliveryTime.
    */
   public static int getMessageOrdinal(Metaclass metaclass, Timestamp deliveryTime, ActionContext actx)
   {
      if (!deliveryTime.equals(s_tsLastTimestamp))
      {
         s_tsLastTimestamp = deliveryTime;
         s_nOrdinal = 0;
      }
      else
      {
         s_nOrdinal++;
      }

      return s_nOrdinal;
   }

   /**
    * @deprecated Complete a message delivery.
    * @param oid The oid of the message that has been delivered.
    */
   public static void complete(Metaclass metaclass, InvocationContext context, final ObjectQueueDispatcher comp, final OID oid,
      ActionContext actx)
   {
      complete(metaclass, comp, oid, actx);
   }

   /**
    * Complete a message delivery.
    * 
    * @param oid The oid of the message that has been delivered.
    */
   public void complete(OID oid)
   {
      Binary binOID = oid.toBinary();
      String sTransactionNode = (String)m_nodeByProcessingMessageIdMap.get(binOID);

      if (sTransactionNode != null)
      {
         Set transactionSet = (Set)m_processingMessagesByNodeNameMap.get(sTransactionNode);

         if (transactionSet != null)
         {
            transactionSet.remove(binOID);
         }

         m_nodeByProcessingMessageIdMap.remove(sTransactionNode);
      }
   }

   /**
    * @return Set of resources that have saturated semaphores.
    */
   public Set getBlockingSet()
   {
      return m_blockingSet;
   }

   /**
    * Sets the invocation context component.
    * 
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * Sets the metadata.
    * 
    * @param metadata The metadata.
    */
   public void setMetadata(Metadata metadata)
   {
      m_metadata = metadata;
   }

   /**
    * Sets the enabled flag.
    * 
    * @param enabled The enabled flag.
    */
   public void setEnabled(boolean bEnabled)
   {
      m_bEnabled = bEnabled;
   }

   /**
    * @return true if the dispatcher is enabled.
    */
   public boolean isEnabled()
   {
      return m_bEnabled;
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public synchronized void shutdown()
   {
      m_logger.info("Stopping ObjectQueueDispatcher");
      m_bShutdown = true;
      m_logger.dump("Shutdown notifying all threads");
      notifyAll();
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public synchronized void startup() throws Exception
   {
      if (m_metadata == null)
      {
         // support older metadata that did not initialize through component
         // definition
         m_metadata = Repository.getMetadata();
      }

      m_channel = (ObjectDispatcherQueue)m_metadata.getChannel("SysObjectQueueDispatcher");
      m_bShutdown = !m_bEnabled;
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      startup();
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      shutdown();
   }

   /**
    * Notify that all messages are processed.
    */
   public void notifyCompletion()
   {
      synchronized (m_syncTestLock)
      {
         m_syncTestLock.notifyAll();
      }
   }

   /**
    * Wait until all messages are processed.
    */
   public void waitForCompletion()
   {
      synchronized (m_syncTestLock)
      {
         try
         {
            m_syncTestLock.wait(1000);
         }
         catch (InterruptedException e)
         {
            // Ignore.
         }
      }
   }

   /**
    * Get whether object consumer pool is started and the current thread is the main unit test thread.
    * @return Whether object consumer pool is started and the current thread is the main unit test thread.
    */
   public synchronized boolean isTestStarted()
   {
      if (m_factory == null || m_bShutdown || !UnitTestPlayer.isMainThread())
      {
         return false;
      }

      ObjectQueueConnection con = null;

      try
      {
         con = m_factory.open();

         return con.isStarted();
      }
      catch (IntegrationException e)
      {
         throw e;
      }
      catch (Exception e)
      {
         throw new RPCException("err.rpc.msg", e);
      }
      finally
      {
         if (con != null)
         {
            con.close();
         }
      }
   }

   // inner classes

   /**
    * A semaphore protects a resource accessible by a limited number of
    * concurrent threads.
    */
   private class Semaphore
   {
      /**
       * The maximum concurrency count.
       */
      public int m_nMaxCount;

      /**
       * The current concurrency count.
       */
      public int m_nCount;

      /**
       * Acquire the semaphore.
       * 
       * @param maxCount The maximum count of the semaphore.
       * @return true if the semaphore is acquired, false otherwise.
       */
      public boolean acquire(Number maxCount)
      {
         if (maxCount == null)
         {
            m_nCount++;

            return true;
         }

         m_nMaxCount = maxCount.intValue();

         if (m_nCount < m_nMaxCount)
         {
            m_nCount++;

            return true;
         }

         return false;
      }

      /**
       * @return true If acquire(m_nMaxCount) will fail.
       */
      public boolean isSaturated()
      {
         return m_nCount >= m_nMaxCount;
      }

      /**
       * Releases the semaphore, return true if this is no longer held by any
       * thread.
       */
      public boolean release()
      {
         m_nCount--;

         return m_nCount <= 0;
      }
   }

   /**
    * Class to associate a command with a priority.
    */
   private class Delivery implements Comparable
   {
      public DispatcherMessage m_message;

      public int m_nPriority;

      public int m_nOrdinal;

      public boolean m_bSynchronous;

      public Delivery()
      {
      }

      public Delivery(int nPriority, int nOrdinal, DispatcherMessage message, boolean bSynchronous)
      {
         m_message = message;
         m_nPriority = nPriority;
         m_nOrdinal = nOrdinal;
         m_bSynchronous = bSynchronous;
      }

      public boolean isUnavailable()
      {
         return m_message == null;
      }

      /**
       * Initiate delivery of message to a node.
       * 
       * @param sNode the node to which to deliver.
       */
      public void deliver(String sNode)
      {
         if (m_message.getType() != DispatcherMessage.RECEIVE)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("GetMessage delivering non-persisted message to node " + sNode);
            }
         }
         else
         {
            Binary binOID = (Binary)m_message.getArgArray()[0];

            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("GetMessage delivering oid " + binOID + " to node " + sNode);
            }

            // record transaction, so we can track the most recent node to
            // operate on the message.
            m_nodeByProcessingMessageIdMap.put(binOID, sNode);

            if (sNode != null)
            {
               Set transactionSet = (Set)m_processingMessagesByNodeNameMap.get(sNode);

               if (transactionSet == null)
               {
                  transactionSet = new HashHolder(1);
                  m_processingMessagesByNodeNameMap.put(sNode, transactionSet);
               }

               transactionSet.add(binOID);
            }
         }
      }

      /**
       * @see java.lang.Comparable#compareTo(java.lang.Object)
       */
      public int compareTo(Object req)
      {
         int nOtherPriority = ((Delivery)req).m_nPriority;

         if (m_nPriority < nOtherPriority)
         {
            return -1;
         }

         if (m_nPriority > nOtherPriority)
         {
            return 1;
         }

         int nOtherOrdinal = ((Delivery)req).m_nOrdinal;

         if (m_nOrdinal < nOtherOrdinal)
         {
            return -1;
         }

         if (m_nOrdinal > nOtherOrdinal)
         {
            return 1;
         }

         return 0;
      }
   }

   /**
    * Heap full exception.
    */
   public static class HeapFullException extends UncheckedException
   {
      // constants

      /**
       * Serial version id.
       */
      private final static long serialVersionUID = -585433540858179976L;

      // attributes

      /**
       * Whether to notify the dispatcher.
       */
      private boolean m_bNotifyDispatcher;

      // constructors

      /**
       * Construct a heap full exception.
       * @param sErrCode Error code.
       */
      public HeapFullException(String sErrCode)
      {
         super(sErrCode);
      }

      // operations

      /**
       * @return Whether to notify the dispatcher.
       */
      public boolean isNotifyDispatcher()
      {
         return m_bNotifyDispatcher;
      }

      /**
       * @param bNotifyDispatcher Whether to notify the dispatcher.
       */
      public void setNotifyDispatcher(boolean bNotifyDispatcher)
      {
         m_bNotifyDispatcher = bNotifyDispatcher;
      }
   }
}