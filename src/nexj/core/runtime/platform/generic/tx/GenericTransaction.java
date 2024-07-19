// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime.platform.generic.tx;

import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.RollbackException;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.util.Binary;
import nexj.core.util.Logger;
import nexj.core.util.RandUtil;

/**
 * Generic transaction implementation.
 */
public class GenericTransaction implements Transaction
{
   // attributes
   
   /**
    * The transaction status.
    */
   protected int m_nStatus = Status.STATUS_ACTIVE;

   /**
    * The synchronization object count.
    */
   protected int m_nSyncCount;

   /**
    * The transaction id.
    */
   protected byte[] m_txId;

   /**
    * The branch qualifier.
    */
   protected long m_lBranch;
   
   /**
    * The global transaction prefix.
    */
   protected static long s_lPrefix;

   /**
    * The global transaction suffix.
    */
   protected final static byte[] s_suffix = new byte[16];

   static
   {
      RandUtil.getSecureRandom().nextBytes(s_suffix);
   }

   // associations

   /**
    * The first branch.
    */
   protected Branch m_firstBranch;
   
   /**
    * The last branch.
    */
   protected Branch m_lastBranch;
   
   /**
    * The array of transaction synchronization objects.
    */
   protected Synchronization[] m_syncArray;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericTransaction.class);

   // constructors
   
   public GenericTransaction()
   {
      long lPrefix;
      
      synchronized (GenericXid.class)
      {
         lPrefix = s_lPrefix++;
      }
      
      m_txId = new byte[24];
      
      setLong(m_txId, 0, lPrefix);
      System.arraycopy(s_suffix, 0, m_txId, 8, 16);
   }

   // operations

   /**
    * @see javax.transaction.Transaction#commit()
    */
   public synchronized void commit() throws RollbackException, HeuristicMixedException, HeuristicRollbackException, SecurityException,
      SystemException
   {
      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
            break;

         case Status.STATUS_MARKED_ROLLBACK:
         case Status.STATUS_ROLLING_BACK:
         case Status.STATUS_ROLLEDBACK:
            throw new RollbackException("Attempt to commit transaction " + this +
               " marked for rollback");

         default:
            throw new IllegalStateException("Attempt to commit transaction " + this +
               " that is not active (status=" + m_nStatus + ")");
      }

      syncBeforeCompletion();

      for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
      {
         for (Resource res = branch.firstResource; res != null; res = res.next)
         {
            try
            {
               if (res.state == Resource.SUSPENDED)
               {
                  res.xar.start(branch.xid, XAResource.TMRESUME);
               }
               else if (res.state == Resource.ENLISTED || res.state == Resource.SUSPENDED)
               {
                  res.xar.end(branch.xid, XAResource.TMSUCCESS);
               }

               res.state = Resource.ENDED;
            }
            catch (XAException e)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Error committing transaction " + this +
                     ", unable to end resource " + res, e);
               }

               m_nStatus = Status.STATUS_MARKED_ROLLBACK;
            }
         }
      }
      
      int nHeuristic = 0;

      if (m_nStatus == Status.STATUS_ACTIVE)
      {
         boolean bOnePhase = (m_firstBranch == null || m_firstBranch.next == null);

         if (!bOnePhase)
         {
            m_nStatus = Status.STATUS_PREPARING;
            
            for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
            {
               try
               {
                  branch.readOnly = (branch.firstResource.xar.prepare(branch.xid) == XAResource.XA_RDONLY);
               }
               catch (XAException e)
               {
                  nHeuristic = combineHeuristics(nHeuristic, e.errorCode);
                  branch.forget();

                  if (e.errorCode != XAException.XA_HEURCOM)
                  {
                     m_nStatus = Status.STATUS_MARKED_ROLLBACK;
                     break;
                  }
               }
            }
         }

         if (bOnePhase || m_nStatus != Status.STATUS_MARKED_ROLLBACK &&
            (nHeuristic == 0 || nHeuristic == XAException.XA_HEURCOM))
         {
            m_nStatus = Status.STATUS_COMMITTING;

            for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
            {
               if (!branch.readOnly)
               {
                  try
                  {
                     branch.firstResource.xar.commit(branch.xid, bOnePhase);
                  }
                  catch (XAException e)
                  {
                     nHeuristic = combineHeuristics(nHeuristic, e.errorCode);
                     branch.forget();
   
                     if (bOnePhase)
                     {
                        m_nStatus = Status.STATUS_MARKED_ROLLBACK;
                        break;
                     }
                  }
               }
            }
            
            if (m_nStatus != Status.STATUS_MARKED_ROLLBACK)
            {
               m_nStatus = Status.STATUS_COMMITTED;
            }
         }
      }

      if (m_nStatus == Status.STATUS_COMMITTED)
      {
         syncAfterCompletion(m_nStatus);
         throwHeuristic(nHeuristic);
      }
      else
      {
         m_nStatus = Status.STATUS_ROLLING_BACK;

         for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
         {
            if (!branch.readOnly)
            {
               try
               {
                  branch.firstResource.xar.rollback(branch.xid);
               }
               catch (XAException e)
               {
                  branch.forget();
               }
            }
         }

         syncAfterCompletion(Status.STATUS_ROLLEDBACK);

         throw new RollbackException("Transaction " + this + " rolled back due to previous errors");
      }
   }

   /**
    * @see javax.transaction.Transaction#rollback()
    */
   public synchronized void rollback() throws IllegalStateException, SystemException
   {
      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
            m_nStatus = Status.STATUS_MARKED_ROLLBACK;
            break;
            
         case Status.STATUS_MARKED_ROLLBACK:
            break;
         
         case Status.STATUS_PREPARING:
            m_nStatus = Status.STATUS_MARKED_ROLLBACK;
            return;

         default:
            throw new IllegalStateException("Attempt to rollback transaction " + this + ", that is not active");
      }

      for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
      {
         for (Resource res = branch.firstResource; res != null; res = res.next)
         {
            try
            {
               if (res.state == Resource.SUSPENDED)
               {
                  res.xar.start(branch.xid, XAResource.TMRESUME);
               }
               else if (res.state == Resource.ENLISTED || res.state == Resource.SUSPENDED)
               {
                  res.xar.end(branch.xid, XAResource.TMSUCCESS);
               }

               res.state = Resource.ENDED;
            }
            catch (XAException e)
            {
               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Error rolling back transaction " + this +
                     ", unable to end resource " + res, e);
               }
            }
         }
      }

      m_nStatus = Status.STATUS_ROLLING_BACK;

      for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
      {
         if (!branch.readOnly)
         {
            try
            {
               branch.firstResource.xar.rollback(branch.xid);
            }
            catch (XAException e)
            {
               branch.forget();
            }
         }
      }

      syncAfterCompletion(Status.STATUS_ROLLEDBACK);
   }

   /**
    * @see javax.transaction.Transaction#setRollbackOnly()
    */
   public synchronized void setRollbackOnly() throws IllegalStateException, SystemException
   {
      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
         case Status.STATUS_PREPARING:
         case Status.STATUS_PREPARED:
            m_nStatus = Status.STATUS_MARKED_ROLLBACK;
            break;

         case Status.STATUS_MARKED_ROLLBACK:
         case Status.STATUS_ROLLING_BACK:
            break;

         default:
            throw new IllegalStateException("Attempt to mark for rollback transaction " + this +
               " that is neither active, nor preparing, nor prepared (status=" + m_nStatus + ")"); 
      }
   }

   /**
    * @see javax.transaction.Transaction#getStatus()
    */
   public int getStatus()
   {
      return m_nStatus;
   }

   /**
    * @see javax.transaction.Transaction#enlistResource(javax.transaction.xa.XAResource)
    */
   public synchronized boolean enlistResource(XAResource xar) throws RollbackException, IllegalStateException, SystemException
   {
      assert xar != null;

      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
         case Status.STATUS_PREPARING:
            break;
         
         case Status.STATUS_MARKED_ROLLBACK:
         case Status.STATUS_ROLLING_BACK:
         case Status.STATUS_ROLLEDBACK:
            throw new RollbackException("Attempt to enlist a resource in transaction " + this +
               " marked for rollback");

         default:
            throw new IllegalStateException("Attempt to enlist a resource in transaction " + this +
               " that is neither active, nor preparing (status=" + m_nStatus + ")");
      }
      
      try
      {
         Branch branch;
         boolean bNewBranch = false;

         for (branch = m_firstBranch; branch != null; branch = branch.next)
         {
            if (xar.isSameRM(branch.firstResource.xar))
            {
               break;
            }
         }

         if (branch == null)
         {
            branch = new Branch(createXid());
            bNewBranch = true;
         }

         Resource res;

         for (res = branch.firstResource; res != null; res = res.next)
         {
            if (res.xar == xar)
            {
               if (res.state == Resource.ENLISTED)
               {
                  return false;
               }

               break;
            }
         }

         if (bNewBranch)
         {
            xar.start(branch.xid, XAResource.TMNOFLAGS);
            addBranch(branch);
         }
         else if (res != null && res.state == Resource.SUSPENDED)
         {
            xar.start(branch.xid, XAResource.TMRESUME);
         }
         else
         {
            try
            {
               xar.start(branch.xid, XAResource.TMJOIN);
            }
            catch (XAException e)
            {
               if (e.errorCode == XAException.XAER_INVAL ||
                  e.errorCode == XAException.XAER_RMERR)
               {
                  // jTDS (XAER_INVAL) and Oracle (XAER_RMERR) kluge
                  branch = new Branch(createXid());
                  res = null;
                  xar.start(branch.xid, XAResource.TMNOFLAGS);
                  addBranch(branch);
               }
               else
               {
                  throw e;
               }
            }
         }

         if (res == null)
         {
            res = new Resource(xar, branch);
            branch.addResource(res);
         }

         res.state = Resource.ENLISTED;
      }
      catch (XAException e)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Error (" + e.errorCode + ") enlisting resource " +
               xar + " in transaction " + this, e);
         }

         return false;
      }

      return true;
   }

   /**
    * @see javax.transaction.Transaction#delistResource(javax.transaction.xa.XAResource, int)
    */
   public synchronized boolean delistResource(XAResource xar, int nFlag) throws IllegalStateException, SystemException
   {
      assert xar != null;
      assert nFlag == XAResource.TMSUSPEND || nFlag == XAResource.TMFAIL || nFlag == XAResource.TMSUCCESS;

      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
            break;

         default:
            throw new IllegalStateException("Attempt to delist a resource from transaction " + this +
               " that is not active (status=" + m_nStatus + ")");
      }

      try
      {
         Resource res = null;
         
      search:
         for (Branch branch = m_firstBranch; branch != null; branch = branch.next)
         {
            for (res = branch.firstResource; res != null; res = res.next)
            {
               if (res.xar == xar)
               {
                  break search;
               }
            }
         }
         
         if (res == null)
         {
            return false;
         }

         res.xar.end(res.branch.xid, nFlag);
         
         if (nFlag == XAResource.TMSUSPEND)
         {
            res.state = Resource.SUSPENDED;
         }
         else
         {
            res.state = Resource.ENDED;

            if (nFlag == XAResource.TMFAIL)
            {
               m_nStatus = Status.STATUS_MARKED_ROLLBACK;
            }
         }
      }
      catch (XAException e)
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Error delisting resource " + xar + " from transaction " + this, e);
         }

         m_nStatus = Status.STATUS_MARKED_ROLLBACK;

         return false;
      }

      return true;
   }

   /**
    * @see javax.transaction.Transaction#registerSynchronization(javax.transaction.Synchronization)
    */
   public synchronized void registerSynchronization(Synchronization sync) throws RollbackException, IllegalStateException, SystemException
   {
      assert sync != null;

      switch (m_nStatus)
      {
         case Status.STATUS_ACTIVE:
         case Status.STATUS_PREPARING:
            break;
         
         case Status.STATUS_MARKED_ROLLBACK:
         case Status.STATUS_ROLLING_BACK:
         case Status.STATUS_ROLLEDBACK:
            throw new RollbackException("Attempt to register synchronization with transaction " + this +
               " marked for rollback");
        
         default:
            throw new IllegalStateException("Attempt to register synchronization with transaction " + this +
               " that is neither active, nor preparing (status=" + m_nStatus + ")");
      }

      if (m_syncArray == null)
      {
         m_syncArray = new Synchronization[8];
      }
      else if (m_nSyncCount == m_syncArray.length)
      {
         Synchronization[] syncArray = new Synchronization[m_nSyncCount << 1];
         
         System.arraycopy(m_syncArray, 0, syncArray, 0, m_nSyncCount);
         m_syncArray = syncArray;
      }

      m_syncArray[m_nSyncCount++] = sync;
   }

   /**
    * Notifies the synchronization listeners before transaction completion.
    */
   protected void syncBeforeCompletion()
   {
      try
      {
         for (int i = 0; i < m_nSyncCount; ++i)
         {
            m_syncArray[i].beforeCompletion();
         }
      }
      catch (Throwable t)
      {
         if (s_logger.isWarnEnabled())
         {
            s_logger.warn("Unexpected error before completion of transaction " + this, t);
         }

         m_nStatus = Status.STATUS_MARKED_ROLLBACK;
      }
   }

   /**
    * Notifies the synchronization listeners after transaction completion.
    */
   protected void syncAfterCompletion(int nStatus)
   {
      m_nStatus = Status.STATUS_NO_TRANSACTION; 

      for (int i = 0; i < m_nSyncCount; ++i)
      {
         try
         {
            m_syncArray[i].afterCompletion(nStatus);
         }
         catch (Throwable t)
         {
            if (s_logger.isWarnEnabled())
            {
               s_logger.warn("Unexpected error after completion of transaction " + this, t);
            }
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public synchronized String toString()
   {
      StringBuffer buf = new StringBuffer(64);
      
      buf.append("Tx(");
      Binary.append(buf, m_txId, -1);
      buf.append(")");
      
      return buf.toString();
   }

   /**
    * @return A new Xid.
    */
   protected Xid createXid()
   {
      byte[] brq = new byte[Xid.MAXBQUALSIZE];

      setLong(brq, 0, m_lBranch++);

      return new GenericXid(m_txId, brq);
   }

   /**
    * Adds a new branch to the transaction.
    */
   protected void addBranch(Branch branch)
   {
      if (m_firstBranch == null)
      {
         m_firstBranch = m_lastBranch = branch;
      }
      else
      {
         m_lastBranch.next = branch;
         m_lastBranch = branch;
      }
   }

   /**
    * Combines old and new heuristic codes.
    * @param nOld The old code.
    * @param nNew The new code.
    * @return The combined code.
    */
   protected static int combineHeuristics(int nOld, int nNew)
   {
      if (nOld == 0)
      {
         return nNew;
      }
      
      switch (nNew)
      {
         case XAException.XA_HEURCOM:
            if (nOld == XAException.XA_HEURHAZ || nOld == XAException.XA_HEURRB)
            {
               return XAException.XA_HEURMIX;
            }
            
            return nOld;
            
         case XAException.XA_HEURHAZ:
            if (nOld == XAException.XA_HEURCOM || nOld == XAException.XA_HEURRB)
            {
               return XAException.XA_HEURMIX;
            }
            
            return nOld;
            
         case XAException.XA_HEURMIX:
            return nNew;
         
         case XAException.XA_HEURRB:
            if (nOld == XAException.XA_HEURCOM || nOld == XAException.XA_HEURHAZ)
            {
               return XAException.XA_HEURMIX;
            }
            
            return nOld;
         
         default:
            return nOld;
      }
   }
   
   /**
    * Throws if necessary a heuristic exception based on an error code.
    * @param nHeuristic The heuristic error code.
    */
   protected void throwHeuristic(int nHeuristic) throws HeuristicMixedException, HeuristicRollbackException
   {
      switch (nHeuristic)
      {
         case XAException.XA_HEURHAZ:
         case XAException.XA_HEURMIX:
            throw new HeuristicMixedException("Heuristic mixed result of transaction " + this);
         
         case XAException.XA_HEURRB:
            throw new HeuristicRollbackException("Heuristic rollback of transaction " + this);
      }
   }

   /**
    * Stores a long number at a given offset in a byte array.
    * @param data The destination byte array.
    * @param nOffset The offset in the array, from which to store the data.
    * @param l The data to store.
    */
   protected static void setLong(byte[] data, int nOffset, long l)
   {
      data[nOffset] = (byte)l;
      data[nOffset + 1] = (byte)(l >> 8);
      data[nOffset + 2] = (byte)(l >> 16);
      data[nOffset + 3] = (byte)(l >> 24);
      data[nOffset + 4] = (byte)(l >> 32);
      data[nOffset + 5] = (byte)(l >> 40);
      data[nOffset + 6] = (byte)(l >> 48);
      data[nOffset + 7] = (byte)(l >> 56);
   }

   // inner classes

   /**
    * Data structure for keeping the transaction branches.
    */
   protected static class Branch
   {
      // attributes
      
      /**
       * True if the branch is read-only.
       */
      public boolean readOnly;

      // associations

      /**
       * The transaction branch Xid.
       */
      public Xid xid;
      
      /**
       * The next branch.
       */
      public Branch next;

      /**
       * The first resource in the list.
       */
      public Resource firstResource;
      
      /**
       * The last resource in the list.
       */
      public Resource lastResource;
      
      // constructors

      /**
       * Creates the branch.
       * @param xid The branch Xid.
       * @param resource The first resource.
       */
      public Branch(Xid xid)
      {
         this.xid = xid;
      }

      // operations

      /**
       * Adds a resource to the tail of the resource list.
       * @param resource The resource to add.
       */
      public void addResource(Resource resource)
      {
         if (this.firstResource == null)
         {
            this.firstResource = this.lastResource = resource;
         }
         else
         {
            this.lastResource.next = resource;
            this.lastResource = resource;
         }
      }
      
      /**
       * Forgets the branch.
       */
      public void forget()
      {
         this.readOnly = true;
         
         try
         {
            this.firstResource.xar.forget(this.xid);
         }
         catch (Throwable t)
         {
         }
      }
   }

   /**
    * Data structure for keeping the XA resources and their states.
    */
   protected static class Resource
   {
      // constants

      /**
       * The resource has been just created and initialized.
       */
      protected final static int INITIALIZED = 0;

      /**
       * The resource has been enlisted.
       */
      protected final static int ENLISTED = 1;

      /**
       * The resource has been suspended.
       */
      protected final static int SUSPENDED = 2;

      /**
       * The resource has been ended.
       */
      protected final static int ENDED = 3;

      // attributes
      
      /**
       * One of the above constants.
       */
      public int state = INITIALIZED;
      
      // associations
      
      /**
       * The XA resource.
       */
      public XAResource xar;
      
      /**
       * The transaction branch.
       */
      public Branch branch;
      
      /**
       * The next resource.
       */
      public Resource next;

      // constructors

      /**
       * Constructs the resource.
       */
      public Resource(XAResource xar, Branch branch)
      {
         this.xar = xar;
         this.branch = branch;
      }
   }
}
