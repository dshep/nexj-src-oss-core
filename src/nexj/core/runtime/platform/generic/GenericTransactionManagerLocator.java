// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic;

import javax.transaction.TransactionManager;

import nexj.core.runtime.platform.TransactionManagerLocator;
import nexj.core.runtime.platform.generic.tx.GenericTransactionManager;

/**
 * Generic transaction manager locator.
 */
public class GenericTransactionManagerLocator implements TransactionManagerLocator
{
   // associations

   /**
    * The transaction manager singleton.
    */
   protected final static GenericTransactionManager s_manager = new GenericTransactionManager();

   // operations

   /**
    * @see nexj.core.runtime.platform.TransactionManagerLocator#getTransactionManager()
    */
   public TransactionManager getTransactionManager() throws Exception
   {
      return s_manager;
   }
}
