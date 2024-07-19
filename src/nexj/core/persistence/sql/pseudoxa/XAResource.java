// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql.pseudoxa;

import java.sql.Connection;
import java.sql.SQLException;

import javax.transaction.xa.XAException;
import javax.transaction.xa.Xid;

import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * An Object implementing the transactional functionality of an XAResource for a non-XA DataSoure
 * on a best-effort basis.
 * Every branch has a separate SQL Connection object to allow for suspend/resume.
 */
public class XAResource implements javax.transaction.xa.XAResource
{
   /**
    * The given XID is currently the active XID (at most one XID can have this state at a time)
    */
   protected final static Object STATE_ACTIVE = new Object();

   /**
    * The given XID is in a commit-pending state, set via end(Xid,  TMSUCCESS)
    */
   protected final static Object STATE_COMMIT = new Object();

   /**
    * The given XID is in a rollback-pending state, set via end(Xid, TMFAIL)
    */
   protected final static Object STATE_ROLLBACK = new Object();

   /**
    * The given XID is currently suspended.
    */
   protected final static Object STATE_SUSPEND = new Object();

   /**
    * The last Xid.
    */
   protected Xid m_lastXid;

   /**
    * Map of known Xid branch states (TRUE == TMSUCCESS, FALSE == TMFAIL, null == still active).
    */
   protected Lookup/*<Xid, STATE_*>*/ m_xidStateMap = new HashTab/*<Xid, STATE_*>*/(1);

   /**
    * The wrapped connection to use for managing transactions.
    */
   protected Connection m_connection;

   /**
    * Constructor.
    * @param connection The wrapped connection to use for managing transactions.
    */
   public XAResource(Connection connection)
   {
      assert connection != null;

      m_connection = connection;
   }

   /**
    * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
    */
   public void commit(Xid xid, boolean bOnePhase) throws XAException
   {
      Object state = m_xidStateMap.get(xid);

      if (state == null)
      {
         throw new XAException(XAException.XAER_NOTA); // invalid Xid to commit
      }

      if (state != STATE_ACTIVE && state != STATE_SUSPEND && state != STATE_COMMIT)
      {
         throw new XAException(XAException.XAER_INVAL); // trying to commit a rolled-back branch
      }

      releaseXid(xid, true);
   }

   /**
    * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
    */
   public void end(Xid xid, int nFlags) throws XAException
   {
      // sequence: .end() -> .prepare() -> .commit()/.rollback()
      Object state = m_xidStateMap.get(xid);

      if (nFlags == TMSUSPEND) //the transaction branch is temporarily suspended in incomplete state
      {
         if (state != STATE_ACTIVE || !m_lastXid.equals(xid))
         {
            throw new XAException(XAException.XAER_NOTA); // invalid Xid to suspend
         }

         m_xidStateMap.put(xid, STATE_SUSPEND); // set Xid to suspended state
      }
      else if (nFlags == TMFAIL) // the portion of work has failed
      {
         if (state != STATE_ACTIVE && state != STATE_SUSPEND)
         {
            throw new XAException(XAException.XAER_NOTA); // invalid Xid to fail
         }

         m_xidStateMap.put(xid, STATE_ROLLBACK); // set Xid to rollback state
      }
      else if (nFlags == TMSUCCESS) // the portion of work has completed successfully
      {
         if (state != STATE_ACTIVE && state != STATE_SUSPEND)
         {
            throw new XAException(XAException.XAER_NOTA); // invalid Xid to fail
         }

         m_xidStateMap.put(xid, STATE_COMMIT); // set Xid to successful state
      }
      else
      {
         throw new XAException(XAException.XAER_INVAL); // invalid arguments
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
    */
   public void forget(Xid xid) throws XAException
   {
      rollback(xid); // forgetting is same as rolling back and releasing connection
   }

   /**
    * @see javax.transaction.xa.XAResource#getTransactionTimeout()
    */
   public int getTransactionTimeout() throws XAException
   {
      return 0; // Transaction Timeout not supported
   }

   /**
    * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
    */
   public boolean isSameRM(javax.transaction.xa.XAResource xares) throws XAException
   {
      return false; // TMJOIN not supported by this implementation, see start(Xid, int)
   }

   /**
    * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
    */
   public int prepare(Xid xid) throws XAException
   {
      // sequence: .end() -> .prepare() -> .commit()/.rollback()
      if (m_xidStateMap.get(xid) != STATE_COMMIT)
      {
         throw new XAException(XAException.XAER_INVAL); // not a successfully ended Xid
      }

      return XAResource.XA_OK; // vote ready to commit, no way of verifying this with RDBMS
   }

   /**
    * @see javax.transaction.xa.XAResource#recover(int)
    */
   public Xid[] recover(int nFlag) throws XAException
   {
      return null; // recovery not supported
   }

   /**
    * Release all resources associated with an Xid.
    * @param xid The Xid to release resources for.
    * @param bCommit Commit the connections, false == rollback connections.
    * @throws XAException On inconsistent branch state.
    */
   protected void releaseXid(Xid xid, boolean bCommit) throws XAException
   {
      if (m_lastXid == xid)
      {
         m_lastXid = null; // reset last active Xid
      }

      try
      {
         if (bCommit)
         {
            m_connection.commit();
         }
         else
         {
            m_connection.rollback();
         }
      }
      catch (SQLException e)
      {
         XAException ex = new XAException(XAException.XAER_RMERR);

         ex.initCause(e);

         try
         {
            m_connection.close();
         }
         catch (SQLException x)
         {
            // Ignore exception
         }

         throw ex;
      }
      finally
      {
         m_xidStateMap.remove(xid);
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
    */
   public void rollback(Xid xid) throws XAException
   {
      if (!m_xidStateMap.contains(xid))
      {
         throw new XAException(XAException.XAER_NOTA); // invalid Xid to rollback
      }

      releaseXid(xid, false);
   }

   /**
    * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
    */
   public boolean setTransactionTimeout(int nSeconds) throws XAException
   {
      return false; // Transaction Timeout not supported
   }

   /**
    * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
    */
   public void start(Xid xid, int nFlags) throws XAException
   {
      if (nFlags == TMJOIN)
      {
         if (m_lastXid == null)
         {
            throw new XAException(XAException.XAER_NOTA); // nothing to join with
         }

         // the start applies to joining a transaction previously seen by the resource manager
         // Javadoc does not constrain if xid itself needs to have been seen previously or not
         // However, Javadoc does state that TMJOIN cannot resume a suspended transaction because:
         // "If TMSUSPEND is specified ... must be resumed via the start method with TMRESUME
         //  specified"
         // If the xid itself has been seen previously this will cause problems when joining two
         // suspended transactions. Hence to simplify implementation do not support joining XIDs.
         if (!m_lastXid.equals(xid))
         {
            // this should never get hit since isSameRM() returns false, thereby preventing joins
            throw new UnsupportedOperationException();
         }

         // Do not resume a transaction since TMRESUME not specified
      }
      else if (nFlags == TMRESUME) // Javadoc does not require that there be no active Xids
      {
         if (m_xidStateMap.get(xid) != STATE_SUSPEND)
         {
            throw new XAException(XAException.XAER_INVAL); // not one of the suspended Xids
         }

         // if the last active Xid is still active then silently suspend it
         if (m_lastXid != null && m_xidStateMap.get(m_lastXid) == STATE_ACTIVE)
         {
            m_xidStateMap.put(m_lastXid, STATE_ACTIVE);
         }

         m_xidStateMap.put(xid, STATE_ACTIVE);
         m_lastXid = xid; // set as last Xid
      }
      else if (nFlags == TMNOFLAGS) // Javadoc does not require that there be no active Xids
      {
         if (m_xidStateMap.contains(xid))
         {
            // the transaction specified by Xid has previously been seen by the resource manager,
            // the resource manager throws the XAException exception with XAER_DUPID error code
            throw new XAException(XAException.XAER_DUPID);
         }

         // do not support more than one Xid per XAResource (to simplify branch management)
         if (m_lastXid != null)
         {
            throw new UnsupportedOperationException(); // refuse starting another Xid on XAResource
         }

         // if the last active Xid is still active then silently suspend it
         if (m_lastXid != null && m_xidStateMap.get(m_lastXid) == STATE_ACTIVE)
         {
            m_xidStateMap.put(m_lastXid, STATE_ACTIVE);
         }

         m_xidStateMap.put(xid, STATE_ACTIVE);
         m_lastXid = xid; // set as last Xid

         try
         {
            m_connection.setAutoCommit(false); // transactions should not auto-commit
         }
         catch (SQLException e)
         {
            XAException x = new XAException(XAException.XAER_RMERR);

            x.initCause(e);

            throw x;
         }
      }
      else
      {
         throw new XAException(XAException.XAER_INVAL); // invalid arguments
      }
   }
}