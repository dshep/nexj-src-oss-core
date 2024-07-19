// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra.engine;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.Session;

import nexj.core.rpc.jms.ra.JMSConsumerConfig;
import nexj.core.util.PropertiesAware;

/**
 * Used to perform operations in a JMS engine provider specific way.
 */
public interface JMSEngineAdapter extends PropertiesAware
{
   /**
    * @param consumerConfig The consumer config activation spec.
    * @return The connection consumer max messages parameter to use.
    */
   public int getConnectionConsumerMaxMessageCount(JMSConsumerConfig consumerConfig);
   
   /**
    * @param consumerConfig The consumer config activation spec.
    * @return The max number of concurrent consumers to use in a consumer pool.
    */
   public int getMaxPoolSize(JMSConsumerConfig consumerConfig);
   
   /**
    * @param bTransacted True to indicate that the channel is transacted.
    * @param bRecovery True to return a JMS connection factory to be used for recovery only.
    * @return The JMS ConnectionFactory object specific to a JMS engine provider.
    */
   public Object getJMSConnectionFactory(boolean bTransacted, boolean bRecovery) throws Throwable;

   /**
    * @return True if the return value from XAResource isSameRM() should be used.
    * False to make the return value from isSameRM() always false (ActiveMQ kludge).
    */
   public boolean isSameRMUsed();

   /**
    * This method is used to enable a JMS session to share its transactional state with other
    * JMS sessions in the case when multiple sessions are involved in a managed transaction.
    * The sharing is achieved either through a common XAResource in case of an XA transaction
    * or through some other implementation specific way in case of a local transaction.
    * 
    * @param newJMSSession The JMS session on which to enable the shared transaction behaviour.
    * @param sharedJMSSession The JMS session on which to model the shared transaction behaviour.
    *                         i.e. which contains the XAResource to be shared or some other,
    *                         implementation specific transaction sharing mechanism.
    * @throws Throwable
    */
   public void setSharedTransactions(Session newJMSSession, Session sharedJMSSession) throws Throwable;
   
   /**
    * This method can be used to perform any JMS engine provider specific
    * transformation required on a message before processing.
    * 
    * @param message The message to perform transformations on.
    * @return The transformed message.
    * @throws Throwable
    */
   public Message prepareMessage(Message message) throws Throwable;

   /**
    * This method is used to perform any JMS engine specific transformation on the session
    * 
    * @param session - session that was created from a connection
    * @return a physical session
    */
   public Session getSession(Session session) throws JMSException;
}
