package nexj.core.util.pool.consumer;

import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.TransactionManager;

import nexj.core.util.Logger;

/**
 * Transactional request consumer.
 */
public abstract class TransactionalConsumer extends GenericConsumer
{
   // associations

   /**
    * The request being processed. Cached for possible rejection in finish().
    */
   protected Object m_request;

   // constructors

   /**
    * Constructs the consumer.
    * @param pool The consumer pool.
    * @param logger The class logger.
    */
   public TransactionalConsumer(GenericConsumerPool pool, Logger logger) throws Throwable
   {
      super(pool, logger);
   }

   // operations

   /**
    * Sets the transaction timeout for this invocation of the consumer.
    * @see nexj.core.util.pool.consumer.GenericConsumer#run()
    */
   public void run()
   {
      TransactionManager tm = null;

      try
      {
         int nTimeout = m_config.getTransactionTimeout();

         if (nTimeout != 0)
         {
            tm = m_pool.getAdapter().getTransactionManager();

            if (tm != null)
            {
               if (nTimeout < 0)
               {
                  nTimeout = Integer.MAX_VALUE;
               }

               try
               {
                  // Sets the transaction timeout for this thread only.
                  // However, for WebSphere the timeout is set in the J2EE descriptor as well. 
                  tm.setTransactionTimeout(nTimeout);
               }
               catch (SystemException e)
               {
                  m_logger.error("Unable to set the transaction timeout in " + this, e);
               }
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

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#consume(java.lang.Object)
    */
   protected void consume(Object request)
   {
      m_request = request;
      super.consume(request);
   }

   /**
    * @see nexj.core.util.pool.consumer.GenericConsumer#finish(boolean)
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
                     m_logger.debug("Rolling back the local transaction");
                  }

                  rollback();
               }
               else
               {
                  if (m_logger.isDebugEnabled())
                  {
                     m_logger.debug("Committing the local trsnaction");
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
            reject(m_request);
         }
      }
   }

   /**
    * Completes an XA transaction, based on the m_bFailed flag.
    */
   protected void complete() throws Exception
   {
      TransactionManager tm = m_pool.getAdapter().getTransactionManager();
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
    * @see nexj.core.util.pool.consumer.GenericConsumer#deactivate()
    */
   public void deactivate()
   {
      m_request = null;
      super.deactivate();
   }
}
