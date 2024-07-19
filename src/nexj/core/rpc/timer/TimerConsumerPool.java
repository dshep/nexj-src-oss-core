package nexj.core.rpc.timer;

import java.util.Iterator;

import nexj.core.meta.integration.channel.timer.Timer;
import nexj.core.rpc.pool.ChannelConsumerPool;
import nexj.core.util.BinaryHeap;
import nexj.core.util.ComparableComparator;
import nexj.core.util.Heap;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.GenericConsumer;

/**
 * Timer consumer pool.
 */
public class TimerConsumerPool extends ChannelConsumerPool
{
   // associations

   /**
    * The timeout heap: Timeout[].
    */
   protected final Heap m_heap = new BinaryHeap(ComparableComparator.INSTANCE);

   // constructors

   /**
    * Constructs the consumer pool.
    * @param timer The timer metadata.
    * @param adapter The consumer adapter.
    */
   public TimerConsumerPool(Timer timer, ConsumerAdapter adapter)
   {
      super(timer, adapter);
   }

   // operations

   /**
    * Schedules a timeout.
    * @param timeout The timeout to schedule.
    */
   public void schedule(Timeout timeout)
   {
      long lTime = timeout.getTime();

      if (lTime != Long.MAX_VALUE && !timeout.isCancelationRequested())
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Scheduling " + timeout);
         }

         synchronized (this)
         {
            Object first = m_heap.first();

            m_heap.add(timeout);

            if (m_heap.first() != first)
            {
               notifyAll();
            }
         }
      }
      else
      {
         synchronized (timeout)
         {
            timeout.setCanceled(true);
            timeout.notifyAll();
         }
      }
   }

   /**
    * Cancels a timeout.
    * @param timeout The timeout to cancel.
    * @param bWait True to block waiting for the cancelation.
    */
   public void cancel(Timeout timeout, boolean bWait)
   {
      timeout.setCancelationRequested(true);

      synchronized (this)
      {
         if (m_heap.remove(timeout) || !bWait)
         {
            return;
         }
      }

      synchronized (timeout)
      {
         while (!timeout.isCanceled())
         {
            try
            {
               timeout.wait();
            }
            catch (InterruptedException e)
            {
            }
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#connect()
    */
   protected void connect() throws Throwable
   {
      for (Iterator itr = ((Timer)m_config).getTimeoutIterator(); itr.hasNext();)
      {
         schedule(new Timeout((nexj.core.meta.integration.channel.timer.Timeout)itr.next()));
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#suspend()
    */
   protected void suspend()
   {
      close();
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#close()
    */
   protected void close()
   {
      Timeout[] timeoutArray;

      synchronized (this)
      {
         timeoutArray = new Timeout[m_heap.size()];

         int i = 0;

         for (Iterator itr = m_heap.iterator(); itr.hasNext();)
         {
            timeoutArray[i++] = (Timeout)itr.next();
         }
      }

      for (int i = 0; i < timeoutArray.length; ++i)
      {
         cancel(timeoutArray[i], false);
      }
   }

   /**
    * @see nexj.core.rpc.ra.GenericConsumerPool#listen()
    */
   protected void listen()
   {
      Timeout timeout = (Timeout)m_heap.first();

      try
      {
         if (timeout == null)
         {
            wait();
         }
         else
         {
            long lDelay = timeout.getTime() - System.currentTimeMillis();

            if (lDelay > 0)
            {
               wait(lDelay);
            }
         }
      }
      catch (InterruptedException e)
      {
      }

      if (!m_bStop)
      {
         long lNow = System.currentTimeMillis();

         for (;;)
         {
            timeout = (Timeout)m_heap.first();

            if (timeout == null || timeout.getTime() > lNow)
            {
               break;
            }

            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Expired " + timeout);
            }

            m_heap.removeFirst();

            TimerConsumer consumer = null;

            try
            {
               consumer = (TimerConsumer)getConsumer();
               consumer.start(timeout);
               consumer = null;
            }
            catch (Throwable t)
            {
               handleException(t);
            }
            finally
            {
               if (consumer != null)
               {
                  m_heap.add(timeout);
                  consumer.dispose();
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumerPool#createConsumer()
    */
   protected GenericConsumer createConsumer() throws Throwable
   {
      return new TimerConsumer(this);
   }
}
