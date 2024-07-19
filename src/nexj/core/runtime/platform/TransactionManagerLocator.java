// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform;

import javax.transaction.TransactionManager;

/**
 * Interface implemented by transaction manager locators.
 */
public interface TransactionManagerLocator
{
   /**
    * @return The transaction manager.
    */
   TransactionManager getTransactionManager() throws Exception;
}
