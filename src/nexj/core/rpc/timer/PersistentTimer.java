// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.timer;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import nexj.core.integration.SecureSender;
import nexj.core.integration.Sender;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Metadata;
import nexj.core.meta.Repository;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.ChannelType;
import nexj.core.meta.integration.channel.timer.Timer;
import nexj.core.monitoring.Counter;
import nexj.core.monitoring.jmx.StatManager;
import nexj.core.monitoring.jmx.StatManagerAware;
import nexj.core.persistence.DeadlockException;
import nexj.core.persistence.Query;
import nexj.core.rpc.TransferObject;
import nexj.core.rpc.jms.JMSSender;
import nexj.core.rpc.pool.Processor;
import nexj.core.rpc.queueing.ObjectSender;
import nexj.core.runtime.Initializable;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.ThreadContextHolder;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Lifecycle;
import nexj.core.util.Logger;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerPool;
import nexj.core.util.pool.consumer.ConsumerPoolProvider;

/**
 * Generic persistent timer implementation.
 * Must be instantiated as a singleton component per node.
 */
public class PersistentTimer implements ConsumerPoolProvider, StatManagerAware, Processor, Initializable, Lifecycle
{
   // attributes

   /**
    * The default user name.
    */
   protected String m_sDefaultUser;

   /**
    * The polling interval in ms.
    */
   protected long m_lInterval = 60000;

   /**
    * The SysTimer batch count.
    */
   protected int m_nBatchCount = 8;

   /**
    * The next timeout in ms since 1970-01-01 00:00:00 UTC.
    */
   protected long m_lTimeout;

   /**
    * The last deferral time.
    */
   protected long m_lDeferralTime;

   /**
    * True if the timer is operating in a cluster.
    */
   protected boolean m_bDistributed;

   /**
    * Flag to stop the timer.
    */
   protected boolean m_bStop;

   /**
    * True if the timer has been stopped (i.e. finished stopping).
    */
   protected boolean m_bStopped = true;

   // associations

   /**
    * The channel for the timer configuration.
    */
   protected Timer m_channel = new Timer("SystemTimer");

   /**
    * The invocation context component.
    */
   protected Component m_contextComponent;

   /**
    * This component.
    */
   protected Component m_thisComponent;

   /**
    * The asynchronous message queue client.
    */
   protected Sender m_queueClient;

   /**
    * The asynchronous message broadcast client.
    */
   protected Sender m_topicClient;

   /**
    * SystemPartition class object.
    */
   protected Metaclass m_systemPartitionClass;

   /**
    * Total timeout count.
    */
   protected Counter m_timeoutCount; 

   /**
    * Total persistent timeout count.
    */
   protected Counter m_persistentCount; 

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(PersistentTimer.class);

   /**
    * The system log enabler.
    */
   protected final static Logger s_enabler = Logger.getSystemLogger(PersistentTimer.class);

   // operations

   /**
    * Sets the distributed system flag.
    * @param bDistributed True if the system is operating in a cluster.
    */
   public void setDistributed(boolean bDistributed)
   {
      m_bDistributed = bDistributed;
   }

   /**
    * Sets the default user account.
    * @param sUser The user account name.
    */
   public void setDefaultUser(String sUser)
   {
      m_sDefaultUser = sUser;
   }

   /**
    * Sets the maximum polling interval.
    * @param lInterval The maximum polling interval in ms.
    */
   public void setInterval(long lInterval)
   {
      m_lInterval = lInterval;
   }

   /**
    * Sets the timer batch count (maximum number of timers processed in one transaction).
    * @param nCount The batch count.
    */
   public void setBatchCount(int nCount)
   {
      m_nBatchCount = nCount;
   }

   /**
    * Sets the asynchronous message queue client.
    * @param queueClient The asynchronous message queue client to set.
    */
   public void setQueueClient(Sender queueClient)
   {
      m_queueClient = queueClient;
   }

   /**
    * Sets the asynchronous message broadcast client.
    * @param topicClient The asynchronous message broadcast client to set.
    */
   public void setTopicClient(Sender topicClient)
   {
      m_topicClient = topicClient;
   }

   /**
    * Set system partition class.
    * @param systemPartitionClass System partition class.
    */
   public void setSystemPartitionClass(Metaclass systemPartitionClass)
   {
      m_systemPartitionClass = systemPartitionClass;
   }

   /**
    * Sets the invocation context component.
    * @param contextComponent The invocation context component to set.
    */
   public void setContextComponent(Component contextComponent)
   {
      m_contextComponent = contextComponent;
   }

   /**
    * Sets this component.
    * @param component The component to set.
    */
   public void setThisComponent(Component component)
   {
      m_thisComponent = component;
   }

   /**
    * Adds a timeout.
    * @param timeout The timeout to add.
    */
   public void addTimeout(nexj.core.meta.integration.channel.timer.Timeout timeout)
   {
      m_channel.addTimeout(timeout);
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

      ChannelType type = new ChannelType("Timer");

      type.setMetadata(m_thisComponent.getMetadata());

      m_channel.setType(type);
      m_channel.setReceivable(true);
      m_channel.setReceiver(m_thisComponent);
      m_channel.addTimeout(new nexj.core.meta.integration.channel.timer.Timeout());
   }

   /**
    * @see nexj.core.monitoring.jmx.StatManagerAware#setStatManager(nexj.core.monitoring.jmx.StatManager)
    */
   public void setStatManager(StatManager statManager)
   {
      m_timeoutCount = (Counter)statManager.defineStatistic(m_channel.getReceiverStatPath(),
         "Generic Timeout Count", "counter", true, Channel.STAT_PERSIST_CLASS).getStatistic();

      m_persistentCount = (Counter)statManager.defineStatistic(m_channel.getReceiverStatPath(),
         "Persistent Timeout Count", "counter", true, Channel.STAT_PERSIST_CLASS).getStatistic();
   }

   /**
    * @see nexj.core.util.Lifecycle#startup()
    */
   public synchronized void startup() throws Exception
   {
      m_bStop = false;
   }

   /**
    * @see nexj.core.util.Lifecycle#shutdown()
    */
   public synchronized void shutdown()
   {
      // Request stop
      m_bStop = true;
      notifyAll();

      // Wait until stopped
      while (!m_bStopped)
      {
         try
         {
            wait();
         }
         catch (InterruptedException ex)
         {
         }
      }
   }

   /**
    * @see nexj.core.util.Suspendable#suspend()
    */
   public void suspend() throws Exception
   {
      shutdown();
   }

   /**
    * @see nexj.core.util.Suspendable#resume()
    */
   public void resume() throws Exception
   {
      startup();
   }

   /**
    * Resets the timer next timeout.
    * @param lTimeout The timeout in ms since 1970-01-01.
    * @param bNotify Whether to notify all threads that the timeout was reset.
    */
   protected synchronized void reset(long lTimeout, boolean bNotify)
   {
      if (m_lTimeout == 0 || lTimeout < m_lTimeout)
      {
         m_lTimeout = lTimeout;

         if (bNotify)
         {
            notifyAll();
         }
      }
   }

   /**
    * Notifies the timer about a new timeout.
    * @param lTimeout The timeout in ms since 1970-01-01.
    */
   public void notify(long lTimeout)
   {
      reset(lTimeout, true);
   }

   /**
    * Defers the polling by another interval.
    */
   public synchronized void defer()
   {
      m_lDeferralTime = System.currentTimeMillis();

      long lTimeout = m_lDeferralTime + m_lInterval;

      if (lTimeout > m_lTimeout)
      {
         m_lTimeout = lTimeout;
      }
   }

   /**
    * @see nexj.core.rpc.pool.Processor#process(java.lang.Object)
    */
   public void process(Object request) throws Throwable
   {
      if (m_timeoutCount != null)
      {
         m_timeoutCount.add(1);
      }

      Timeout timeout = (Timeout)request;
      Component receiver = timeout.getReceiver();

      if (receiver != null && receiver != m_thisComponent)
      {
         Object obj = receiver.getInstance(null);

         if (obj instanceof TimeoutProcessor)
         {
            long lNext = ((TimeoutProcessor)obj).timeout();

            if (lNext != 0)
            {
               timeout.setTime(lNext);
            }
         }
         else
         {
            ((Processor)obj).process(request);
         }

         return;
      }

      synchronized (this)
      {
         m_bStopped = false;
      }

      boolean bLogEnabledSaved = Logger.isEnabled();

      s_enabler.enable();

      try
      {
         if (m_lTimeout != 0)
         {
            synchronized (this)
            {
               long lDelay = m_lTimeout - System.currentTimeMillis();

               // Limit the delay to avoid thread pool warnings.
               // The tolerance is needed to avoid too frequent invocation. 
               if (lDelay > 90000)
               {
                  lDelay = 60000;
               }

               if (m_bStop)
               {
                  timeout.setTime(Long.MAX_VALUE);

                  return;
               }

               if (lDelay > 0)
               {
                  wait(lDelay);
               }

               if (m_bStop)
               {
                  timeout.setTime(Long.MAX_VALUE);

                  return;
               }

               if (System.currentTimeMillis() < m_lTimeout)
               {
                  timeout.setTime(Long.MIN_VALUE);

                  return;
               }

               m_lTimeout = 0;
            }
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Polling timer \"" +  m_thisComponent.getName() + "\"");
         }

         InvocationContext context = (InvocationContext)m_contextComponent.getInstance(null);
         int nCookie = -1;

         try
         {
            context.initialize(null);
            context.setSecure(false);

            if (m_systemPartitionClass != null)
            {
               context.setPartitioned(false);

               Instance systemPartition = Query.createRead(m_systemPartitionClass, null, null, null, 1, 0, false,
                     Query.SEC_NONE, context).read().getInstance(0);

               context.login((Instance)systemPartition.getValue("systemUser"));
               context.setPartitioned(true);
            }

            nCookie = Logger.pushContext(context.getPrincipal().getName());

            Metadata metadata = context.getMetadata();
            Metaclass timerClass = metadata.getMetaclass("SysTimer");
            Pair attributes =
               new Pair(Symbol.define("next"),
               new Pair(Symbol.define("start"),
               new Pair(Symbol.define("period"),
               new Pair(Symbol.define("principal"),
               new Pair(Symbol.define("data"))))));
            Pair orderBy = new Pair(new Pair(Symbol.define("next"), Boolean.TRUE));
            List sendList = null;

            if (m_queueClient instanceof InvocationContextAware)
            {
               ((InvocationContextAware)m_queueClient).setInvocationContext(context);
            }

         loop:
            for (;;)
            {
               context.beginTransaction();
               context.getUnitOfWork().setRaw(true);

               long lCurrentTime = System.currentTimeMillis();
               InstanceList list = Query.createRead(timerClass, attributes,
                  (m_bDistributed) ? Pair.attribute("next").le(new Timestamp(lCurrentTime + m_lInterval)) : null,
                  orderBy, m_nBatchCount, 0, true, Query.SEC_NONE, context).read();

               for (int i = 0; i < list.size(); ++i)
               {
                  Instance timer = list.getInstance(i);
                  long lNextTimeout = ((Timestamp)timer.getValue("next")).getTime();

                  if (lNextTimeout <= lCurrentTime)
                  {
                     String sId = timer.getOID().getValue(0).toString();
                     boolean bJMS = m_queueClient instanceof JMSSender;
                     TransferObject tobj = (bJMS) ? new TransferObject(2) :
                           new TransferObject("ObjectQueueMessage", 4);
                     TransferObject properties = (bJMS) ? new TransferObject(3) : null;
                     TransferObject p = (bJMS) ? properties : tobj;

                     p.setValue("timer", sId);
                     p.setValue(SecureSender.PROTECTED, Boolean.FALSE);

                     Object user = timer.getValue("principal");

                     if (user == null)
                     {
                        user = m_sDefaultUser;
                     }

                     if (user != null)
                     {
                        p.setValue(SecureSender.USER, user);
                     }

                     if (s_logger.isDebugEnabled())
                     {
                        s_logger.debug("Timeout in timer \"" +  m_thisComponent.getName() +
                           "\", id=" + sId + "; forwarding to the message queue");
                     }

                     tobj.setValue(Sender.BODY, ((Binary)timer.getValue("data")).toObject());

                     if (bJMS)
                     {
                        tobj.setValue(JMSSender.PROPERTIES, properties);
                     }

                     if (sendList == null)
                     {
                        sendList = new ArrayList(m_nBatchCount);
                     }

                     sendList.add(tobj);

                     long lPeriod = ((Number)timer.getValue("period")).longValue();

                     if (lPeriod <= 0)
                     {
                        timer.invoke("delete");
                     }
                     else
                     {
                        Timestamp tmStart = (Timestamp)timer.getValue("start");

                        lNextTimeout = tmStart.getTime() + lPeriod * ((lCurrentTime - tmStart.getTime()) / lPeriod + 1);
                        timer.setValue("next", new Timestamp(lNextTimeout));
                        reset(lNextTimeout, false);
                     }
                  }
                  else
                  {
                     reset(lNextTimeout, false);

                     break loop;
                  }
               }

               if (list.size() < m_nBatchCount)
               {
                  break;
               }

               if (sendList != null)
               {
                  if (m_persistentCount != null)
                  {
                     m_persistentCount.add(sendList.size());
                  }

                  m_queueClient.send(sendList);
                  sendList = null;
               }

               context.complete(true);
               context.initUnitOfWork();
            }

            if (sendList != null)
            {
               m_queueClient.send(sendList);
               sendList = null;
            }

            // Defer the timeout in the other nodes

            if (m_bDistributed)
            {
               synchronized (this)
               {
                  long lCurrentTime = System.currentTimeMillis();

                  if (m_lTimeout - lCurrentTime >= (m_lInterval >> 1) ||
                     lCurrentTime - m_lDeferralTime >= (m_lInterval >> 1))
                  {
                     m_lDeferralTime = lCurrentTime;

                     if (m_topicClient != null)
                     {
                        TransferObject properties = new TransferObject(1);

                        properties.setValue(JMSSender.NODE, J2EEUtil.NODE_ID);

                        TransferObject tobj = new TransferObject(2);

                        tobj.setValue(JMSSender.BODY, new DeferTimeoutCommand(m_thisComponent.getName()));
                        tobj.setValue(JMSSender.PROPERTIES, properties);

                        m_topicClient.send(tobj);
                     }
                     else
                     {
                        TransferObject tobj = new TransferObject("ObjectQueueMessage", 2);

                        tobj.setValue(Sender.BODY, new DeferTimeoutCommand(m_thisComponent.getName()));
                        tobj.setValue(ObjectSender.RECEIVER, Symbol.define("other"));

                        m_queueClient.send(tobj);
                     }
                  }
               }
            }

            context.complete(true);
         }
         catch (Throwable t)
         {
            context.complete(false);

            throw t;
         }
         finally
         {
            if (nCookie != -1)
            {
               Logger.resetContext(nCookie);
            }

            ThreadContextHolder.setContext(null);
            reset((m_bDistributed) ? System.currentTimeMillis() + m_lInterval : Long.MAX_VALUE, false);
         }
      }
      catch (InterruptedException e)
      {
         if (s_logger.isInfoEnabled())
         {
            s_logger.info("Timer \"" + m_thisComponent.getName() + "\" interrupted");
         }
      }
      catch (Throwable e)
      {
         s_logger.log((e instanceof DeadlockException) ? Logger.DEBUG : Logger.ERROR, "Unexpected timer exception", e);
         reset(System.currentTimeMillis() + ((m_lInterval + 7) >> 3), false);
      }
      finally
      {
         synchronized (this)
         {
            m_bStopped = true;
            notifyAll();
         }

         Logger.setEnabled(bLogEnabledSaved);
      }

      timeout.setTime(Long.MIN_VALUE);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#createConsumerPool(nexj.core.util.pool.consumer.ConsumerAdapter)
    */
   public ConsumerPool createConsumerPool(ConsumerAdapter adapter)
   {
      return m_channel.createConsumerPool(adapter);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#setConsumerPool(nexj.core.util.pool.consumer.ConsumerPool)
    */
   public void setConsumerPool(ConsumerPool pool)
   {
      m_channel.setConsumerPool(pool);
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#getConsumerPool()
    */
   public ConsumerPool getConsumerPool()
   {
      return m_channel.getConsumerPool();
   }
}
