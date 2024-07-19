package nexj.core.meta.integration.channel.queueing;

/**
 * Channel for the system message dispatcher.
 */
public class ObjectDispatcherQueue extends ObjectQueue
{
   // attributes

   /**
    * The dispatcher port.
    */
   protected int m_nPort = 1111;

   /**
    * The max senders.
    */
   protected int m_nMaxSenders = 16;

   /**
    * The max receivers.
    */
   protected int m_nMaxReceivers = 16;

   // constructors

   /**
    * @param sName
    */
   public ObjectDispatcherQueue(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * Sets the max receivers.
    * @param nMaxReceivers The max receivers to set.
    */
   public void setMaxReceivers(int nMaxReceivers)
   {
      verifyNotReadOnly();
      m_nMaxReceivers = nMaxReceivers;
   }

   /**
    * @return The max receivers.
    */
   public int getMaxReceivers()
   {
      return m_nMaxReceivers;
   }

   /**
    * Sets the max senders.
    * @param nMaxSenders The max senders to set.
    */
   public void setMaxSenders(int nMaxSenders)
   {
      verifyNotReadOnly();
      m_nMaxSenders = nMaxSenders;
   }

   /**
    * @return The max senders.
    */
   public int getMaxSenders()
   {
      return m_nMaxSenders;
   }

   /**
    * Sets the dispatcher port.
    * @param nPort The dispatcher port to set.
    */
   public void setPort(int nPort)
   {
      verifyNotReadOnly();
      m_nPort = nPort;
   }

   /**
    * @return The dispatcher port.
    */
   public int getPort()
   {
      return m_nPort;
   }
}
