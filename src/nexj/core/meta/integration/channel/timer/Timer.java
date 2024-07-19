package nexj.core.meta.integration.channel.timer;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.integration.Channel;
import nexj.core.rpc.timer.TimerConsumerPool;
import nexj.core.util.pool.consumer.ConsumerAdapter;
import nexj.core.util.pool.consumer.ConsumerPool;
import nexj.core.util.pool.consumer.ConsumerPoolProvider;

/**
 * Timer channel metadata.
 */
public class Timer extends Channel implements ConsumerPoolProvider
{
   // attributes

   /**
    * The timeout collection.
    */
   protected List m_timeoutList = new ArrayList(8); // of type Timeout

   // constructors

   /**
    * Constructs the channel.
    * @param sName The channel name.
    */
   public Timer(String sName)
   {
      super(sName);
      m_bStealth = true;
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#getMaxReceivers()
    */
   public int getMaxReceivers()
   {
      return -1;
   }

   /**
    * Adds a new timeout to the timer.
    * @param timeout The timeout to add.
    */
   public void addTimeout(Timeout timeout)
   {
      verifyNotReadOnly();
      m_timeoutList.add(timeout);
   }

   /**
    * Gets a timeout by ordinal number.
    * @param nOrdinal The timeout ordinal number (0-based).
    * @return The timeout object.
    */
   public Timeout getTimeout(int nOrdinal)
   {
      return (Timeout)m_timeoutList.get(nOrdinal);
   }

   /**
    * @return The timeout count.
    */
   public int getTimeoutCount()
   {
      return m_timeoutList.size();
   }

   /**
    * @return An iterator for the contained timeout objects.
    */
   public Iterator getTimeoutIterator()
   {
      return m_timeoutList.iterator();
   }

   /**
    * @see nexj.core.util.pool.consumer.ConsumerPoolProvider#createConsumerPool(nexj.core.util.pool.consumer.ConsumerAdapter)
    */
   public ConsumerPool createConsumerPool(ConsumerAdapter adapter)
   {
      return new TimerConsumerPool(this, adapter);
   }
}
