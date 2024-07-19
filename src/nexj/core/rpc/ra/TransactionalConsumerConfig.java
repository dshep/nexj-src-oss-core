// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.resource.spi.InvalidPropertyException;

/**
 * Transactional consumer endpoint configuration.
 */
public class TransactionalConsumerConfig extends GenericConsumerConfig
{
   // attributes

   /**
    * The timeout to use for transactions started by a transactional incoming
    * message adapter, in seconds. Should be 0 to use server default.
    */
   protected int m_nTransactionTimeout;


   // operations

   /**
    * Sets the timeout to use for transactions started on the J2EE component.
    * 
    * @param nTimeout The timeout in seconds; 0 to use server default.
    */
   public void setTransactionTimeout(int nTimeout)
   {
      m_nTransactionTimeout = nTimeout;
   }

   /**
    * Gets the timeout to use for transactions started on the J2EE component.
    * 
    * @return The timeout in seconds; 0 to use server default.
    */
   public int getTransactionTimeout()
   {
      return m_nTransactionTimeout;
   }

   /**
    * @see javax.resource.spi.ActivationSpec#validate()
    */
   public void validate() throws InvalidPropertyException
   {
      super.validate();

      if (m_nTransactionTimeout < 0)
      {
         throw new InvalidPropertyException("TransactionTimeout is negative");
      }
   }
}
