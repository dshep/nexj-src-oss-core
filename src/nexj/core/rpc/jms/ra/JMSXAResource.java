// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import javax.jms.Connection;
import javax.jms.Session;
import javax.jms.XAConnection;
import javax.jms.XAConnectionFactory;
import javax.jms.XASession;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.util.Logger;

/**
 * An XAResource wrapper for use during transaction recovery.
 */
public class JMSXAResource implements XAResource
{
   // associations
   
   /**
    * The XA JMS connection factory.
    */
   protected XAConnectionFactory m_jmsXAConnectionFactory;
   
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSXAResource.class);
   
   // constructors
   
   /**
    * Creates a new instance of JMSXAResource.
    * @param jmsEngineAdapter The JMS engine adapter used to retrieve the XA connection factory
    * @throws Throwable
    */
   public JMSXAResource(JMSEngineAdapter jmsEngineAdapter, boolean bTransacted) throws Throwable
   {
      m_jmsXAConnectionFactory = (XAConnectionFactory)jmsEngineAdapter.getJMSConnectionFactory(bTransacted, true);
   }
   
   /**
    * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
    */
   public void commit(Xid xid, boolean bOnePhase) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
         
         jmsXASess.getXAResource().commit(xid, bOnePhase);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
    */
   public void end(Xid xid, int nFlags) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         jmsXASess.getXAResource().end(xid, nFlags);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
    */
   public void forget(Xid xid) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         jmsXASess.getXAResource().forget(xid);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#getTransactionTimeout()
    */
   public int getTransactionTimeout() throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         return jmsXASess.getXAResource().getTransactionTimeout();
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }      
   }

   /**
    * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
    */
   public boolean isSameRM(XAResource xares) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         return jmsXASess.getXAResource().isSameRM(xares);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }      
   }

   /**
    * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
    */
   public int prepare(Xid xid) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
         
         return jmsXASess.getXAResource().prepare(xid);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }      
   }

   /**
    * @see javax.transaction.xa.XAResource#recover(int)
    */
   public Xid[] recover(int bFlag) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         return jmsXASess.getXAResource().recover(bFlag);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }      
   }

   /**
    * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
    */
   public void rollback(Xid xid) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         jmsXASess.getXAResource().rollback(xid);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }
   }

   /**
    * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
    */
   public boolean setTransactionTimeout(int nSeconds) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
         
         return jmsXASess.getXAResource().setTransactionTimeout(nSeconds);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }      
   }

   /**
    * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
    */
   public void start(Xid xid, int nFlags) throws XAException
   {
      XAConnection jmsXAConn = null;
      XASession jmsXASess = null;
      
      try
      {
         jmsXAConn = getXAConnection();
         jmsXASess = getXASession(jmsXAConn);
      
         jmsXASess.getXAResource().start(xid, nFlags);
      }
      finally
      {
         releaseResources(jmsXAConn, jmsXASess);
      }
   }
   
   /**
    * @return The XA JMS connection.
    * @throws XAException
    */
   protected XAConnection getXAConnection() throws XAException
   {
      try
      {
         return (XAConnection)m_jmsXAConnectionFactory.createXAConnection();
      }
      catch (Throwable t)
      {
         throw getXAException("Error creating JMS XAConnection during recovery", t);
      }
   }
   
   /**
    * @param jmsXAConnection The XA JMS connection.
    * @return The XA JMS session
    * @throws XAException
    */
   protected XASession getXASession(XAConnection jmsXAConnection) throws XAException
   {
      try
      {
         return jmsXAConnection.createXASession();
      }
      catch (Throwable t)
      {
         throw getXAException("Error creating JMS XASession during recovery", t);
      }
   }
   
   /**
    * Converts a Throwable into an XAException
    * @param sReason The reason to initialize the XAException with.
    * @param cause The throwable to used as the XAException cause.
    * @return
    */
   protected XAException getXAException(String sReason, Throwable cause)
   {
      XAException xaEx = new XAException(sReason);
      
      xaEx.initCause(cause);
      
      return xaEx;
   }
   
   
   /**
    * Release JMS resources.
    * @param jmsConnection The JMS connection to release.
    * @param jmsSession The JMS session to release.
    */
   protected void releaseResources(Connection jmsConnection, Session jmsSession)
   {
      if (jmsSession != null)
      {
         try
         {
            jmsSession.close();
         }
         catch (Throwable t)
         {
            s_logger.error("Error closing JMS session during recovery: " + jmsSession, t);
         }
      }

      if (jmsConnection != null)
      {
         try
         {
            jmsConnection.close();
         }
         catch (Throwable t)
         {
            s_logger.error("Error closing JMS connection during recovery: " + jmsConnection, t);
         }
      }
   }
}
