// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.TransactionManager;

import nexj.core.util.Logger;

/**
 * Transactional message endpoint consumer.
 */
public abstract class TransactionalConsumer extends GenericConsumer
{
   // associations
   
   /**
    * The message being delivered. Cached for possible rejection in finish().
    */
   protected Object m_message;
   
   // constructors

   /**
    * @see nexj.core.rpc.ra.GenericConsumer(GenericConsumerPool, Logger)
    */
   protected TransactionalConsumer(GenericConsumerPool pool, Logger logger) throws Throwable
   {
      super(pool, logger);
   }

   // operations

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#finish(boolean)
    */
   protected void finish(boolean bXA)
   {
      super.finish(bXA);

      if (m_pool.isTransactional())
      {
         try
         {
            if (bXA)
            {
               complete();
            }
            else
            {
               if (m_bFailed)
               {
                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Rolling back the session");
                  }

                  rollback();
               }
               else
               {
                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Committing the session");
                  }

                  commit();
               }
            }
         }
         catch (Throwable e)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug(((m_bFailed) ? "Rollback" : "Commit") + " failed", e);
            }
         }
      }
      else
      {
         try
         {
            complete();
         }
         catch (Throwable e)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug(((m_bFailed) ? "Rollback" : "Commit") + " failed", e);
            }

            m_bFailed = true;
         }

         if (m_bFailed)
         {
            reject(m_message);
         }
      }
   }

   /**
    * Completes an XA transaction, based on the m_bFailed flag.
    */
   protected void complete() throws Exception
   {
      TransactionManager tm = ((TransactionalResourceAdapter)m_pool.getAdapter()).getTransactionManager();
      int nStatus = tm.getStatus();

      if (nStatus == Status.STATUS_MARKED_ROLLBACK)
      {
         m_bFailed = true;
      }

      if (m_bFailed)
      {
         if (nStatus == Status.STATUS_ACTIVE || nStatus == Status.STATUS_MARKED_ROLLBACK)
         {
            if (m_logger.isDebugEnabled())
            {
               m_logger.debug("Rolling back transaction " + tm.getTransaction());
            }

            tm.rollback();
         }
      }
      else if (nStatus == Status.STATUS_ACTIVE)
      {
         if (m_logger.isDebugEnabled())
         {
            m_logger.debug("Committing transaction " + tm.getTransaction());
         }

         tm.commit();
      }
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericConsumer#deactivate()
    */
   protected void deactivate()
   {
      m_message = null;
      
      super.deactivate();
   }

   /**
    * Commits the local transaction to end the session.
    */
   protected abstract void commit() throws Throwable;

   /**
    * Rolls back the local transaction to end the session.
    */
   protected abstract void rollback() throws Throwable;

   /**
    * Rejects a message after an unsuccessful delivery.
    * @param message The message to reject.
    */
   protected abstract void reject(Object message);

   /**
    * @see nexj.core.rpc.ra.GenericConsumer#consume(java.lang.Object)
    */
   protected void consume(Object message)
   {
      m_message = message;
      
      super.consume(message);
   }

   /**
    * Sets the transaction timeout for this invocation of the consumer.
    * @see nexj.core.rpc.ra.GenericConsumer#run()
    */
   public void run()
   {
      TransactionManager tm = null;

      try
      {
         int nTxTimeout = ((TransactionalConsumerConfig)m_pool.getConfig()).getTransactionTimeout();

         if (nTxTimeout != 0)
         {
            tm = ((TransactionalResourceAdapter)m_pool.getAdapter()).getTransactionManager();

            try
            {
               // Sets the transaction timeout for this thread only.
               // However, for WebSphere the timeout is set in the J2EE descriptor as well. 
               tm.setTransactionTimeout(nTxTimeout);
            }
            catch (SystemException e)
            {
               m_logger.error("Unable to set the transaction timeout in " + this, e);
            }
         }

         super.run();
      }
      finally
      {
         if (tm != null)
         {
            try
            {
               tm.setTransactionTimeout(0);
            }
            catch (SystemException e)
            {
               m_logger.error("Unable to set the transaction timeout in " + this, e);
            }
         }
      }
   }
}
