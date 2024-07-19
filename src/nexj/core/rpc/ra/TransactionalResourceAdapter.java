// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.util.Locale;

import javax.resource.spi.BootstrapContext;
import javax.resource.spi.ResourceAdapterInternalException;
import javax.transaction.TransactionManager;

import nexj.core.runtime.platform.TransactionManagerLocator;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.SysUtil;

/**
 * Resource adapter with support for transactions.
 */
public abstract class TransactionalResourceAdapter extends GenericResourceAdapter
{
   // associations

   /**
    * The JTA transaction manager.
    */
   protected TransactionManager m_transactionManager;

   // constructors
   
   /**
    * Constructs the resource adapter.
    * @param logger The class logger.
    */
   public TransactionalResourceAdapter(Logger logger)
   {
      super(logger);
   }

   // operations

   /**
    * @return The transaction manager.
    */
   public TransactionManager getTransactionManager()
   {
      return m_transactionManager;
   }

   /**
    * @see nexj.core.rpc.ra.GenericResourceAdapter#start(javax.resource.spi.BootstrapContext)
    */
   public void start(BootstrapContext context) throws ResourceAdapterInternalException
   {
      if (!J2EEUtil.isContained())
      {
         throw new ResourceAdapterInternalException("Unknown J2EE container");
      }

      String sContainer = J2EEUtil.getPlatformName();

      try
      {
         m_transactionManager = ((TransactionManagerLocator)Class.forName(
            SysUtil.PACKAGE + ".core.runtime.platform." + sContainer.toLowerCase(Locale.ENGLISH) + '.' +
            sContainer + "TransactionManagerLocator").newInstance()).getTransactionManager();
      }
      catch (Throwable e)
      {
         throw new ResourceAdapterInternalException("Unable to locate the transaction manager", e);
      }

      super.start(context);
   }
}
