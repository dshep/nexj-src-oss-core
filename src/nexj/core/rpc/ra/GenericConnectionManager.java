// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.resource.NotSupportedException;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionEvent;
import javax.resource.spi.ConnectionEventListener;
import javax.resource.spi.ConnectionManager;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionFactory;
import javax.security.auth.Subject;
import javax.transaction.RollbackException;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

import nexj.core.runtime.platform.generic.GenericTransactionManagerLocator;
import nexj.core.util.HashDeque;
import nexj.core.util.HashTab;
import nexj.core.util.HolderDeque;
import nexj.core.util.IdentityHashTab;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;

/**
 * Generic connection manager for non-shared managed connections.
 */
public class GenericConnectionManager implements ConnectionManager, ConnectionEventListener
{
   // constants

   /**
    * Serialization UID. 
    */
   private final static long serialVersionUID = -476371819504880799L;

   /**
    * Connection pool key used for null connection request info values.
    */
   protected final static Object NULL_KEY = new Object();
   
   /**
    * Transaction tracking mode NONE: resource is not transacted.
    */
   public final static int NONE = 0;
   
   /**
    * Transaction tracking mode MANAGED: resource is transacted, but connection manager does not need to associate connections.
    * Enlist connections in transactions when not running in a J2EE container.
    */
   public final static int MANAGED = 1;
   
   /**
    * Transaction tracking mode ASSOCIATED: resource is transacted and connections are associated with their current transactions.
    * This implies:
    * 1. enlist connections in transactions when not running in a J2EE container.
    * 2. a handle to one of the Tx's connections will be returned when requesting new handle
    * 3. a ManagedConnection will not be put back in the pool until the Tx completes
    */
   public final static int ASSOCIATED = 2;
   
   // attributes

   /**
    * Transaction tracking mode.
    */
   protected int m_nTxMode = NONE;

   // associations

   /**
    * Map of currently allocated connections and their associated transactions:
    * HolderDeque[ConnectionAssoc].
    * Non-transactioned connections have a null transaction.
    * Connections in m_txMap map should not be in m_poolMap and vice versa.
    */
   protected Lookup/*<ConnectionAssoc, HolderDeque>*/ m_txMap =
      new HashTab/*<ConnectionAssoc, HolderDeque>*/();

   /**
    * Map of connection keys to a holder deques of managed connections: HolderDeque[ConnectionKey].
    * Each deque is a pool.
    */
   protected Lookup m_poolMap = new HashTab();

   /**
    * Maps a managed connection to an ConnectionAssoc: ConnectionAssoc[ManagedConnection].
    */
   protected Lookup/*<ManagedConnection, ConnectionAssoc>*/ m_connectionMap =
      new IdentityHashTab/*<ManagedConnection, ConnectionAssoc>*/();

   /**
    * Maps managed connections to connection info objects: ConnectionInfo[ManagedConnection].
    */
   protected Lookup m_connectionInfoMap = new IdentityHashTab();

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(GenericConnectionManager.class);

   /**
    * The transaction manager.
    */
   protected final static TransactionManager s_txnManager;

   static
   {
      GenericTransactionManagerLocator locator = new GenericTransactionManagerLocator();
      TransactionManager txnManager = null;

      try
      {
         txnManager = locator.getTransactionManager();
      }
      catch (Exception ex)
      {
         ObjUtil.rethrow(ex);
      }

      s_txnManager = txnManager;
   }

   // constructors

   /**
    * Creates a new connection manager.
    */
   public GenericConnectionManager()
   {
   }

   /**
    * Creates a new connection manager for use outside of a J2EE container.
    * 
    * @param nTxMode One of the constants NONE, MANAGED or ASSOCIATED.
    */
   public GenericConnectionManager(int nTxMode)
   {
      m_nTxMode = nTxMode;
   }

   // operations

   /**
    * Determines whether or not connections created by the given factory
    * should be enlisted in the current transaction.
    * 
    * @param factory The factory used to create the connection.
    * @return True if the connection should be enlisted in the current transaction.
    */
   protected boolean isTransacted(ManagedConnectionFactory factory)
   {
      return m_nTxMode == MANAGED || m_nTxMode == ASSOCIATED;
   }

   /**
    * Determine if connections should be tracked by transaction, implying:
    * 1. a handle to one of Tx's allocated connections will be returned when requesting new handle
    * 2. a ManagedConnection will not be put back in the pool until the Tx completes
    * @param factory The factory to check with for proper flag status.
    * @return True if the connection should be associated/tracked with its transaction.
    */
   protected boolean isTxAssociated(ManagedConnectionFactory factory)
   {
      return m_nTxMode == ASSOCIATED;
   }

   /**
    * @see javax.resource.spi.ConnectionManager#allocateConnection(javax.resource.spi.ManagedConnectionFactory, javax.resource.spi.ConnectionRequestInfo)
    */
   public Object allocateConnection(ManagedConnectionFactory factory, ConnectionRequestInfo cri) throws ResourceException
   {
      ManagedConnection connection = null;
      boolean bDone = false;
      Object handle;
      Transaction txn = null;

      if (isTransacted(factory))
      {
         try
         {
            txn = s_txnManager.getTransaction();
         }
         catch (SystemException ex)
         {
            ObjUtil.rethrow(ex);
         }
      }

      ConnectionAssoc assoc = new ConnectionAssoc(
         (txn == null) ? (Object)Thread.currentThread() : txn,
         new ConnectionKey(factory, (cri instanceof GenericConnectionRequestInfo) ?
         ((GenericConnectionRequestInfo)cri).getManagedConnectionPartition() : NULL_KEY),
         true); // assume connection pooling supported by factory
      Subject subject = getSubject();
      boolean bTxAssociated = false; // the connection is not associated with Tx
      HolderDeque deque = null;

      try
      {
         synchronized (this) // reserve one of the created connections
         {
            if (isTxAssociated(factory)) //if track by Tx check with connections used by current Tx
            {
               deque = (HolderDeque)m_txMap.get(assoc);

               if (deque != null) // connections in-use by TX
               {
                  try
                  {
                     connection = factory.matchManagedConnections(deque, subject, cri);
                  }
                  catch (NotSupportedException e)
                  {
                     assoc.m_bPool = false; // connection pooling not supported
                  }

                  if (connection != null) // reuse connection association from previous allocation
                  {
                     bTxAssociated = true; // assoc.m_handleSet can be empty if handles closed
                     assoc = (ConnectionAssoc)m_connectionMap.get(connection);//get full handle set
                  }
               }
            }

            if (connection == null && assoc.m_bPool) //no connection and pooling possibly supported
            {
               deque = (HolderDeque)m_poolMap.get(assoc.m_key);

               if (deque != null) // unallocated connections available in pool
               {
                  try
                  {
                     connection = factory.matchManagedConnections(deque, subject, cri);
                  }
                  catch (NotSupportedException e)
                  {
                     assoc.m_bPool = false; // connection pooling not supported
                  }

                  if (connection != null)
                  {
                     deque.remove(connection);
                  }
               }
            }
         }

         if (connection == null)
         {
            connection = factory.createManagedConnection(subject, cri); // new connection
            connection.addConnectionEventListener(this);
         }

         if (txn != null && !bTxAssociated) // do not re-enlist same connection
         {
            enlist(connection, txn);
         }

         try // get handle and if error then retry if this is a stale connection from pool
         {
            handle = connection.getConnection(subject, cri);
         }
         catch (ResourceException e)
         {
            destroyConnection(connection);

            if (deque == null || !assoc.m_bPool)
            {
               throw e; // new connection has error
            }

            return allocateConnection(factory, cri); // loop/retry
         }

         synchronized (this)
         {
            assoc.m_handleSet.add(handle);
            m_connectionMap.put(connection, assoc);

            if (!bTxAssociated) // If this is the first handle for this connection
            {
               deque = (HolderDeque)m_txMap.get(assoc); // get the Tx-specific deque

               if (deque == null)
               {
                  deque = new HashDeque();
                  m_txMap.put(assoc, deque);
               }

               deque.addFirst(connection); // add to deque tracking connections per transaction
            }
         }

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug(((deque == null || !assoc.m_bPool) ? "Opened" : "Activated") +
                           " connection " + connection);
         }

         bDone = true;
      }
      finally
      {
         if (!bDone && connection != null)
         {
            try
            {
               connection.destroy();
            }
            catch (Throwable e)
            {
            }
         }
      }

      return handle;
   }

   /**
    * @return The subject from the current thread.
    */
   protected Subject getSubject()
   {
      return null;
   }

   /**
    * @see javax.resource.spi.ConnectionEventListener#connectionClosed(javax.resource.spi.ConnectionEvent)
    */
   public void connectionClosed(ConnectionEvent evt)
   {
      ManagedConnection connection = (ManagedConnection)evt.getSource();

      synchronized (this)
      {
         ConnectionAssoc assoc = (ConnectionAssoc)m_connectionMap.get(connection);

         // do not deallocate connection if a Tx still has a connection referenced
         if (assoc == null || // if connection already freed by e.g. cleanupConnection()
             !assoc.m_handleSet.remove(evt.getConnectionHandle()) || // invalid handle
             (assoc.m_txOrThread != null && isTxAssociated(assoc.m_key.m_factory)) || // Tx with con track
             !assoc.m_handleSet.isEmpty()) // have more handles
         {
            return; // do not deallocate ManagedConnection if not all handles closed
         }
      }

      try
      {
         delist(connection);
      }
      catch (Exception e)
      {
         ObjUtil.rethrow(e);
      }

      cleanupConnection(connection);
   }

   /**
    * Cleans up a connection and returns it to the pool. If pooling is not allowed by the
    * resource adapter, then the connection will be destroyed.
    * @param connection The connection to clean up.
    */
   public void cleanupConnection(ManagedConnection connection)
   {
      Object key;

      synchronized (this)
      {
         ConnectionAssoc assoc = (ConnectionAssoc)m_connectionMap.remove(connection);

         if (assoc == null)
         {
            return;
         }

         HolderDeque deque = (HolderDeque)m_txMap.get(assoc);

         assert deque != null;
         deque.remove(connection);
         key = (assoc.m_bPool) ? assoc.m_key : null;

         if (deque.isEmpty())
         {
            m_txMap.remove(assoc); // remove empty Tx->deque association
         }
      }

      try
      {
         delist(connection); // delist just in case called from clear()
      }
      catch (Throwable e)
      {
         s_logger.error("Unable to delist connection " + connection, e);
         key = null;
      }

      if (key != null)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Deactivating connection " + connection);
         }

         try
         {
            connection.cleanup();
         }
         catch (Throwable e)
         {
            s_logger.error("Unable to cleanup connection " + connection, e);
            key = null;
         }
      }

      /*
       * Destroy the managed connection to release resources if:
       * 1) RA does not support pooling (see assoc.m_bPool in allocateConnection)
       * 2) There was an error cleaning up the connection (above)
       */
      if (key == null)
      {
         destroyConnection(connection);
      }
      else
      {
         synchronized (this)
         {
            HolderDeque deque = (HolderDeque)m_poolMap.get(key);

            if (deque == null)
            {
               deque = new HashDeque();
               m_poolMap.put(key, deque);
            }

            deque.addFirst(connection);
         }
      }
   }

   /**
    * @see javax.resource.spi.ConnectionEventListener#connectionErrorOccurred(javax.resource.spi.ConnectionEvent)
    */
   public void connectionErrorOccurred(ConnectionEvent evt)
   {
      ManagedConnection connection = (ManagedConnection)evt.getSource();

      s_logger.error("Error in connection " + connection + "; closing", evt.getException());

      destroyConnection(connection);
   }

   /**
    * @see javax.resource.spi.ConnectionEventListener#localTransactionCommitted(javax.resource.spi.ConnectionEvent)
    */
   public void localTransactionCommitted(ConnectionEvent evt)
   {
   }

   /**
    * @see javax.resource.spi.ConnectionEventListener#localTransactionRolledback(javax.resource.spi.ConnectionEvent)
    */
   public void localTransactionRolledback(ConnectionEvent evt)
   {
   }

   /**
    * @see javax.resource.spi.ConnectionEventListener#localTransactionStarted(javax.resource.spi.ConnectionEvent)
    */
   public void localTransactionStarted(ConnectionEvent evt)
   {
   }

   /**
    * Closes the pooled connections.
    */
   public synchronized void clear()
   {
      for (Lookup.Iterator/*<ManagedConnection>*/ itr = m_connectionMap.iterator(); itr.hasNext();)
      {
         cleanupConnection((ManagedConnection)itr.next()); // close all open/allocated connections
      }

      for (Lookup.Iterator itr = m_poolMap.valueIterator(); itr.hasNext();)
      {
         for (Iterator/*<ManagedConnection>*/ itr2 = ((HolderDeque)itr.next()).iterator();
              itr2.hasNext();)
         {
            destroyConnection((ManagedConnection)itr2.next());
         }
      }

      assert m_connectionMap.size() == 0;
      assert m_poolMap.size() == 0;
      assert m_txMap.size() == 0;
   }

   /**
    * Delists a connection from the current transaction.
    * 
    * @param connection The connection to delist.
    * @return True if the connection was delisted; false if there was nothing to delist.
    * @throws ResourceException If an error occurs delisting the connection.
    * @throws SystemException If an error occurs delisting the connection.
    */
   protected boolean delist(ManagedConnection connection) throws ResourceException, SystemException
   {
      Transaction transaction = null;

      synchronized (this)
      {
         ConnectionInfo info = (ConnectionInfo)m_connectionInfoMap.remove(connection);

         if (info != null)
         {
            transaction = info.getTransaction();
         }
      }

      if (transaction != null)
      {
         transaction.delistResource(connection.getXAResource(), XAResource.TMSUCCESS);
      }

      return transaction != null;
   }

   /**
    * Destroys a connection and removes it from all pools.
    * @param connection The connection to destroy.
    */
   protected void destroyConnection(ManagedConnection connection)
   {
      // Remove from pool
      synchronized (this)
      {
         ConnectionAssoc assoc = (ConnectionAssoc)m_connectionMap.remove(connection);

         if (assoc == null) // not one of the in-use connections or just got a failure on connection
         {
            // search for the connection in the free connection pool
            for (Lookup.Iterator/*<HolderDeque>*/ itr = m_poolMap.valueIterator(); itr.hasNext();)
            {
               HolderDeque deque = (HolderDeque)itr.next();

               deque.remove(connection);

               if (deque.isEmpty())
               {
                  m_poolMap.remove(itr.getKey()); // remove empty ConnectionKey->deque association
               }
            }
         }
         else // a ConnectionAssoc has all the information required to remove connection from pools
         {
            HolderDeque deque = (HolderDeque)m_txMap.get(assoc);

            if (deque != null) // can be null because removed by destroy()
            {
               deque.remove(connection); // remove connection from Tx Map

               if (deque.isEmpty())
               {
                  m_txMap.remove(assoc); // remove empty Tx->deque association
               }
            }

            if (assoc.m_bPool)
            {
               deque = (HolderDeque)m_poolMap.get(assoc.m_key);

               if (deque != null)
               {
                  deque.remove(connection);
               }
            }
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Closing connection " + connection);
      }

      try
      {
         delist(connection);
      }
      catch (Throwable e)
      {
         s_logger.error("Unable to delist connection " + connection, e);
      }

      try
      {
         connection.destroy();
      }
      catch (Throwable e)
      {
         s_logger.error("Unable to destroy connection " + connection, e);
      }
   }

   /**
    * Enlists a connection with a Tx
    * @param connection The connection to enlist (not null).
    * @param tx The transaction to enlist with (not null).
    * @throws ResourceException On enlistment error.
    */
   protected void enlist(ManagedConnection connection, Transaction tx) throws ResourceException
   {
      ConnectionInfo info;
      XAResource xar = connection.getXAResource();

      try
      {
         tx.enlistResource(xar);

         synchronized (this)
         {
            info = (ConnectionInfo)m_connectionInfoMap.get(connection);

            if (info == null)
            {
               m_connectionInfoMap.put(connection, info = new ConnectionInfo(connection));
            }

            info.setTransaction(tx);
         }

         tx.registerSynchronization(info);
      }
      catch (RollbackException e)
      {
         ObjUtil.rethrow(e);
      }
      catch (SystemException e)
      {
         ObjUtil.rethrow(e);
      }
   }

   // inner classes

   /**
    * Connection key for finding a connection in the pool.
    */
   protected static class ConnectionKey
   {
      // associations

      protected ManagedConnectionFactory m_factory;
      protected Object m_key;

      // constructors

      public ConnectionKey(ManagedConnectionFactory factory, Object key)
      {
         m_factory = factory;
         m_key = (key == null) ? NULL_KEY : key;
      }

      // operations

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_factory.hashCode() << 8 ^ m_key.hashCode();
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof ConnectionKey)
         {
            ConnectionKey key = (ConnectionKey)obj;

            return m_key.equals(key.m_key) && m_factory.equals(key.m_factory);
         }

         return false;
      }
   }

   protected class ConnectionInfo implements Synchronization
   {
      // associations

      /**
       * The managed connection.
       */
      protected ManagedConnection m_connection;

      /**
       * The transaction, if the connection is enlisted in one; otherwise null.
       */
      protected Transaction m_transaction;

      // constructors

      /**
       * Creates a new connection info.
       * @param connection The managed connection.
       */
      public ConnectionInfo(ManagedConnection connection)
      {
         m_connection = connection;
      }

      // operations

      /**
       * Sets the transaction in which the connection is enlisted, if any.
       * @param transaction The transaction in which the connection is enlisted.
       */
      public void setTransaction(Transaction transaction)
      {
         m_transaction = transaction;
      }

      /**
       * Gets the transaction in which the connection is enlisted, if any.
       * @return The transaction in which the connection is enlisted.
       */
      public Transaction getTransaction()
      {
         return m_transaction;
      }

      /**
       * @see javax.transaction.Synchronization#afterCompletion(int)
       */
      public void afterCompletion(int status)
      {
         assert m_transaction != null;

         synchronized (GenericConnectionManager.this)
         {
            // tx finished hence do not need to delist connection from tx, hence remove association
            Object info = m_connectionInfoMap.remove(m_connection);

            if (info == null)
            {
               return;
            }

            assert info == this; // connection no longer in Tx and "this" should be the tracked "info"
         }

         cleanupConnection(m_connection);
      }

      /**
       * @see javax.transaction.Synchronization#beforeCompletion()
       */
      public void beforeCompletion()
      {
      }
   }

   /**
    * Bean with connection associations.
    */
   protected static class ConnectionAssoc
   {
      /**
       * Is pooling supported for this connection.
       */
      public boolean m_bPool;

      /**
       * List of associated handles used for removal (see connectionClosed())
       */
      public Set/*<Object>*/ m_handleSet = new HashSet/*<Object>*/(2);

      /**
       * The connection key.
       */
      public ConnectionKey m_key;

      /**
       * The transaction, if the connection is enlisted in one, otherwise the thread.
       */
      public Object m_txOrThread;

      /**
       * Convenience constructor.
       * @param txOrThread The associated transaction, if the connection is enlisted in one, otherwise the thread.
       * @param key The associated connection key (not null).
       * @param bPool Is pooling supported for this connection.
       */
      public ConnectionAssoc(Object txOrThread, ConnectionKey key, boolean bPool)
      {
         m_bPool = bPool;
         m_key = key;
         m_txOrThread = txOrThread;
      }

      /**
       * @see java.lang.Object#equals(java.lang.Object)
       */
      public boolean equals(Object obj)
      {
         if (obj instanceof ConnectionAssoc)
         {
            ConnectionAssoc assoc = (ConnectionAssoc)obj;

            return m_key.equals(assoc.m_key) && ObjUtil.equal(m_txOrThread, assoc.m_txOrThread);
         }

         return false;
      }

      /**
       * @see java.lang.Object#hashCode()
       */
      public int hashCode()
      {
         return m_key.hashCode() ^ ((m_txOrThread == null) ? 0 : m_txOrThread.hashCode());
      }
   }
}