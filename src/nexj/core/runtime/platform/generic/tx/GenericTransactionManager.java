// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic.tx;

import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.InvalidTransactionException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.Status;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import nexj.core.util.Logger;

/**
 * Generic transaction manager implementation.
 */
public class GenericTransactionManager implements TransactionManager
{
   // associations

   /**
    * The thread local storage containing the transaction.
    */
   protected ThreadLocal m_txTLS = new ThreadLocal();

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericTransactionManager.class);

   // operations

   /**
    * Sets the thread transaction.
    * @param tx The transaction to set.
    */
   protected void setCurrentTransaction(GenericTransaction tx)
   {
      m_txTLS.set(tx);
   }

   /**
    * Sets the thread transaction if the old transaction still matches.
    * @param tx The transaction to set.
    * @param txOld The old transaction.
    */
   protected void setCurrentTransaction(GenericTransaction tx, GenericTransaction txOld)
   {
      if (m_txTLS.get() == txOld)
      {
         m_txTLS.set(tx);
      }
   }

   /**
    * @return The current thread transaction.
    */
   protected GenericTransaction getCurrentTransaction()
   {
      GenericTransaction tx = (GenericTransaction)m_txTLS.get();

      if (tx != null && tx.getStatus() == Status.STATUS_NO_TRANSACTION)
      {
         m_txTLS.set(null);

         return null;
      }

      return tx;
   }

   /**
    * @see javax.transaction.TransactionManager#begin()
    */
   public void begin() throws NotSupportedException, SystemException
   {
      if (getCurrentTransaction() != null)
      {
         throw new NotSupportedException("Attempt to start a nested transaction");
      }

      GenericTransaction tx = new GenericTransaction();

      setCurrentTransaction(tx);

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Starting new transaction " + tx);
      }
   }

   /**
    * @see javax.transaction.TransactionManager#suspend()
    */
   public Transaction suspend() throws SystemException
   {
      GenericTransaction tx = getCurrentTransaction();

      if (tx != null)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Suspending transaction " + tx);
         }

         setCurrentTransaction(null);
      }

      return tx;
   }

   /**
    * @see javax.transaction.TransactionManager#resume(javax.transaction.Transaction)
    */
   public void resume(Transaction tx) throws InvalidTransactionException, IllegalStateException, SystemException
   {
      if (getCurrentTransaction() != null)
      {
         throw new IllegalStateException("Attempt to resume transaction " +
            tx + " while another transaction is active");
      }

      setCurrentTransaction((GenericTransaction)tx);

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Resuming transaction " + tx);
      }
   }

   /**
    * @see javax.transaction.TransactionManager#commit()
    */
   public void commit() throws RollbackException, HeuristicMixedException, HeuristicRollbackException, SecurityException,
      IllegalStateException, SystemException
   {
      GenericTransaction tx = getCurrentTransaction();

      if (tx == null)
      {
         throw new IllegalStateException("No transaction to commit");
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Committing transaction " + tx);
      }

      tx.commit();
      setCurrentTransaction(null, tx);
   }

   /**
    * @see javax.transaction.TransactionManager#rollback()
    */
   public void rollback() throws IllegalStateException, SecurityException, SystemException
   {
      GenericTransaction tx = getCurrentTransaction();

      if (tx == null)
      {
         throw new IllegalStateException("No transaction to roll back");
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Rolling back transaction " + tx);
      }

      tx.rollback();
      setCurrentTransaction(null, tx);
   }

   /**
    * @see javax.transaction.TransactionManager#setRollbackOnly()
    */
   public void setRollbackOnly() throws IllegalStateException, SystemException
   {
      GenericTransaction tx = getCurrentTransaction();

      if (tx == null)
      {
         throw new IllegalStateException("No transaction to mark for rollback");
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Marking for rollback transaction " + tx);
      }

      tx.setRollbackOnly();
   }

   /**
    * @see javax.transaction.TransactionManager#getStatus()
    */
   public int getStatus() throws SystemException
   {
      GenericTransaction tx = getCurrentTransaction();

      if (tx == null)
      {
         return Status.STATUS_NO_TRANSACTION;
      }

      return tx.getStatus();
   }

   /**
    * @see javax.transaction.TransactionManager#getTransaction()
    */
   public Transaction getTransaction() throws SystemException
   {
      return getCurrentTransaction();
   }

   /**
    * @see javax.transaction.TransactionManager#setTransactionTimeout(int)
    */
   public void setTransactionTimeout(int nSeconds) throws SystemException
   {
   }
}
