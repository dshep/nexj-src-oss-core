// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.jboss;

import javax.naming.InitialContext;
import javax.transaction.TransactionManager;

import nexj.core.runtime.platform.TransactionManagerLocator;

/**
 * JBoss transaction manager locator.
 */
public class JBossTransactionManagerLocator implements TransactionManagerLocator
{
   /**
    * @see nexj.core.runtime.platform.TransactionManagerLocator#getTransactionManager()
    */
   public TransactionManager getTransactionManager() throws Exception
   {
      return (TransactionManager)new InitialContext().lookup("java:/TransactionManager");
   }
}
