package nexj.core.util.pool.resource;

import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import nexj.core.util.IdentityHashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * XA-transaction-aware resource pool. 
 */
public abstract class TransactionalResourcePool extends GenericResourcePool
{
   // associations

   /**
    * Resource enlistment map: Enlistment[Resource].
    * Lazy-created.
    */
   protected Lookup m_enlistmentMap;

   /**
    * The transaction manager.
    */
   protected TransactionManager m_transactionManager;

   // operations

   /**
    * Sets the transaction manager.
    * @param manager The transaction manager.
    */
   public void setTransactionManager(TransactionManager manager)
   {
      lock();

      try
      {
         m_transactionManager = manager;
      }
      finally
      {
         unlock();
      }
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#getAssoc()
    */
   protected Object getAssoc()
   {
      if (isTransactional())
      {
         try
         {
            return m_transactionManager.getTransaction();
         }
         catch (SystemException e)
         {
            ObjUtil.rethrow(e);
         }
      }

      return null;
   }

   /**
    * @see GenericResourcePool#createKey(Object, Object)
    */
   protected Key createKey(Object config, Object assoc)
   {
      return super.createKey(config, (isAssociated()) ? (assoc != null) ? assoc : Thread.currentThread() : null);
   }

   /**
    * Enlists a resource in a transaction.
    * @see GenericResourcePool#associate(nexj.core.util.pool.resource.Resource, java.lang.Object)
    */
   protected void associate(Resource resource, Object assoc)
   {
      assert resource != null;

      if (assoc instanceof Transaction)
      {
         Transaction tx = (Transaction)assoc;
         XAResource xar = resource.getXAResource();

         if (xar != null)
         {
            try
            {
               if (!tx.enlistResource(xar))
               {
                  throw new EnlistmentException();
               }

               Enlistment enlistment = new Enlistment(resource, tx);

               lock();

               try
               {
                  if (m_enlistmentMap == null)
                  {
                     m_enlistmentMap = new IdentityHashTab();
                  }

                  Object old = m_enlistmentMap.put(resource, enlistment);

                  if (old != null)
                  {
                     m_enlistmentMap.put(resource, old);
                     tx.delistResource(xar, XAResource.TMSUCCESS);

                     throw new EnlistmentException("err.pool.resource.tx.enlistment.dup");
                  }
               }
               finally
               {
                  unlock();
               }

               tx.registerSynchronization(enlistment);
            }
            catch (EnlistmentException e)
            {
               throw e;
            }
            catch (Throwable t)
            {
               throw new EnlistmentException(t);
            }
         }
      }
   }

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#isAssociated(nexj.core.util.pool.resource.Resource)
    */
   protected boolean isAssociated(Resource resource)
   {
      return m_enlistmentMap != null && m_enlistmentMap.contains(resource);
   }

   /**
    * Delists a resource from its associated transaction.
    * @see GenericResourcePool#dissociate(Resource)
    */
   protected boolean dissociate(Resource resource)
   {
      assert resource != null;

      Transaction tx = null;

      lock();

      try
      {
         if (m_enlistmentMap != null)
         {
            Enlistment enlistment = (Enlistment)m_enlistmentMap.remove(resource);

            if (enlistment != null)
            {
               tx = enlistment.getTransaction();
            }
         }
      }
      finally
      {
         unlock();
      }

      if (tx != null)
      {
         try
         {
            if (!tx.delistResource(resource.getXAResource(), XAResource.TMSUCCESS))
            {
               m_logger.error("Unable to delist " + resource);

               return false;
            }
         }
         catch (Throwable t)
         {
            m_logger.error("Unable to delist " + resource, t);

            return false;
         }
      }

      return true;
   }

   /**
    * @see ResourcePool#isTransactional()
    */
   public boolean isTransactional()
   {
      return true;
   }

   /**
    * @return True to associate the resources with transactions.
    */
   public abstract boolean isAssociated();

   // inner classes

   /**
    * Transaction association of a resource.
    */
   protected class Enlistment implements Synchronization
   {
      // associations

      /**
       * The transactional resource.
       */
      protected Resource m_resource;

      /**
       * The transaction.
       */
      protected Transaction m_transaction;

      // constructors

      /**
       * Constructs the transaction association.
       * @param resource The transactional resource.
       * @param tx The transaction.
       */
      public Enlistment(Resource resource, Transaction tx)
      {
         m_resource = resource;
         m_transaction = tx;
      }

      // operations

      /**
       * @return The resource transaction.
       */
      public Transaction getTransaction()
      {
         return m_transaction;
      }

      /**
       * @see javax.transaction.Synchronization#beforeCompletion()
       */
      public void beforeCompletion()
      {
      }

      /**
       * @see javax.transaction.Synchronization#afterCompletion(int)
       */
      public void afterCompletion(int nStatus)
      {
         boolean bDeactivate;

         lock();

         try
         {
            boolean bEnlisted = (m_enlistmentMap != null && m_enlistmentMap.remove(m_resource) != null);
            Key key = (Key)m_activeMap.get(m_resource);

            bDeactivate = (key == null) ? bEnlisted : !key.isActive();
         }
         finally
         {
            unlock();
         }

         if (bDeactivate)
         {
            deactivate(m_resource);
         }
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         StringBuilder buf = new StringBuilder(64);

         buf.append("Enlistment(resource=");
         buf.append(m_resource);
         buf.append(", transaction=");
         buf.append(m_transaction);
         buf.append(')');

         return buf.toString();
      }
   }
}
