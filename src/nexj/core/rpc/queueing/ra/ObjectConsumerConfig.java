package nexj.core.rpc.queueing.ra;

import nexj.core.rpc.ra.GenericConsumerConfig;

/**
 * Configuration for the DispatcherMessage consumer.
 */
public class ObjectConsumerConfig extends GenericConsumerConfig
{
   // attributes

   /**
    * The message dispatcher port.
    */
   protected int m_nPort;

   // operations
   
   /**
    * Sets the message dispatcher port.
    * @param nPort The message dispatcher port to set.
    */
   public void setPort(int nPort)
   {
      m_nPort = nPort;
   }

   /**
    * @return The message dispatcher port.
    */
   public int getPort()
   {
      return m_nPort;
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericConsumerConfig#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append(super.toString());
      buf.append("(port=\"");
      buf.append(m_nPort);
      buf.append("\")");

      return buf.toString();
   }
}
