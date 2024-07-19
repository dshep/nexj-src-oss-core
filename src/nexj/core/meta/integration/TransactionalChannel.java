// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration;

/**
 * Channel supporting XA transactions.
 */
public abstract class TransactionalChannel extends Channel
{
   // attributes

   /**
    * The timeout for transactions started on the J2EE component, in seconds.
    * 0 to use the J2EE conatiner default. 
    */
   protected int m_nTransactionTimeout;

   // constructors

   /**
    * Constructs the channel.
    * @param sName The channel name.
    */
   public TransactionalChannel(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return true;
   }

   /**
    * Sets the timeout to use for transactions started on the J2EE component.
    * @param nTimeout The timeout in seconds. 0 to use the J2EE container default.
    */
   public void setTransactionTimeout(int nTimeout)
   {
      verifyNotReadOnly();
      m_nTransactionTimeout = nTimeout;
   }

   /**
    * Gets the timeout to use for transactions started on the J2EE component.
    * @return The timeout in seconds. 0 to use the J2EE container default.
    */
   public int getTransactionTimeout()
   {
      return m_nTransactionTimeout;
   }
}
