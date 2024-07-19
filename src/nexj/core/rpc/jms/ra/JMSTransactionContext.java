// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.XASession;
import javax.resource.ResourceException;
import javax.resource.spi.LocalTransaction;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

/**
 * This class is used as a wrapper for LocalTransaction and XAResource interfaces.
 */
public class JMSTransactionContext implements LocalTransaction, XAResource
{
   // attributes
   
   /**
    * Indicates whether a global transaction is in progress.
    */
   protected boolean m_bEnlisted;

   /**
    * @see nexj.core.rpc.jms.ra.engine.JMSEngineAdapter#isSameRMUsed()
    */
   protected boolean m_bSameRMUsed;

   // associations

   /**
    * The underlying physical JMS session that is used as a shared JMS session among all JMS sessions
    * that are used during a local transaction.  All LocalTransaction calls are funneled to it.
    */
   protected Session m_physicalJMSSession;
   
   /**
    * The XAResource that is shared during an XA transaction.
    */
   protected XAResource m_xaResource;

   // constructors

   /**
    * Constructor.
    * physicalJMSSession The wrapped JMSSession.
    * bSameRMUsed Should the return value from isSameRM() of wrapped resource be used.
    */
   public JMSTransactionContext(Session physicalJMSSession, boolean bSameRMUsed)
   {
      m_bSameRMUsed = bSameRMUsed;
      m_physicalJMSSession = physicalJMSSession;

      if (physicalJMSSession instanceof XASession)
      {
         m_xaResource = ((XASession)physicalJMSSession).getXAResource();
      }
   }

   // operations

   public boolean isEnlisted()
   {
      return m_bEnlisted;
   }
   
   /**
    * @see javax.resource.spi.LocalTransaction#begin()
    */
   public void begin() throws ResourceException
   {
      m_bEnlisted = true;
   }

   /**
    * @see javax.resource.spi.LocalTransaction#commit()
    */
   public void commit() throws ResourceException
   {
      try
      {
         m_physicalJMSSession.commit();
      }
      catch (JMSException e)
      {
         throw new ResourceException("Commit failed in JMS session " + m_physicalJMSSession, e);
      }
      finally
      {
         m_bEnlisted = false;
      }
   }

   /**
    * @see javax.resource.spi.LocalTransaction#rollback()
    */
   public void rollback() throws ResourceException
   {
      try
      {
         m_physicalJMSSession.rollback();
      }
      catch (JMSException e)
      {
         throw new ResourceException("Rollback failed in JMS session " + m_physicalJMSSession, e);
      }
      finally
      {
         m_bEnlisted = false;
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
    */
   public void commit(Xid xid, boolean onePhase) throws XAException
   {
      m_xaResource.commit(xid, onePhase);
   }

   /**
    * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
    */
   public void end(Xid xid, int flags) throws XAException
   {
      try
      {
         m_xaResource.end(xid, flags);
      }
      finally
      {
         m_bEnlisted = false;
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
    */
   public void forget(Xid xid) throws XAException
   {
      m_xaResource.forget(xid);
   }

   /**
    * @see javax.transaction.xa.XAResource#getTransactionTimeout()
    */
   public int getTransactionTimeout() throws XAException
   {
      return m_xaResource.getTransactionTimeout();
   }

   /**
    * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
    */
   public boolean isSameRM(XAResource xares) throws XAException
   {
      return m_bSameRMUsed && m_xaResource.isSameRM(xares);
   }

   /**
    * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
    */
   public int prepare(Xid xid) throws XAException
   {
      return m_xaResource.prepare(xid);
   }

   /**
    * @see javax.transaction.xa.XAResource#recover(int)
    */
   public Xid[] recover(int flag) throws XAException
   {
      return m_xaResource.recover(flag);
   }

   /**
    * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
    */
   public void rollback(Xid xid) throws XAException
   {
      m_xaResource.rollback(xid);
   }

   /**
    * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
    */
   public boolean setTransactionTimeout(int seconds) throws XAException
   {
      return m_xaResource.setTransactionTimeout(seconds);
   }

   /**
    * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
    */
   public void start(Xid xid, int flags) throws XAException
   {
      m_xaResource.start(xid, flags);
      
      m_bEnlisted = true;
   }
}
