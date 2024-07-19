// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.XAConnection;
import javax.jms.XASession;
import javax.resource.ResourceException;
import javax.resource.spi.ConnectionRequestInfo;
import javax.resource.spi.LocalTransaction;
import javax.security.auth.Subject;
import javax.transaction.xa.XAResource;

import nexj.core.rpc.jms.ra.engine.JMSEngineAdapter;
import nexj.core.rpc.ra.GenericConnection;
import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.rpc.ra.SharedManagedConnection;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * The managed connection implementation for the JMS resource adapter.
 */
public class JMSManagedConnection extends SharedManagedConnection
{
   // associations
   
   /**
    * An implementation of a JMSEngineAdapter used to perform operations in a JMS engine provider specific way.
    */
   protected JMSEngineAdapter m_jmsEngineAdapter;
   
   /**
    * The physical JMS connection.
    */
   protected Connection m_physicalJMSConnection;
   
   /**
    * The physical JMS session.
    */
   protected Session m_physicalJMSSession;
   
   /**
    * The JMS connection request info.
    */
   protected JMSConnectionRequestInfo m_jmsConnectionRequestInfo;
   
   /**
    * The local transaction and XAResource wrapper.
    */
   protected JMSTransactionContext m_jmsTransactionContext;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSManagedConnection.class);
   
   // constructors
   
   /**
    * @param jmsEngineAdapter An implementation of a JMSEngineAdapter used to perform operations in a JMS engine provider specific way.
    * @param physicalJMSConnection The physical JMS connection.
    * @throws Throwable
    */
   public JMSManagedConnection(JMSEngineAdapter jmsEngineAdapter, Connection physicalJMSConnection, JMSConnectionRequestInfo jmsConnectionRequestInfo) throws Throwable
   {
      m_jmsEngineAdapter = jmsEngineAdapter;
      m_physicalJMSConnection = physicalJMSConnection;
      m_jmsConnectionRequestInfo = jmsConnectionRequestInfo;

      if (m_physicalJMSConnection instanceof XAConnection)
      {
         m_physicalJMSSession = ((XAConnection)m_physicalJMSConnection).createXASession();
      }
      else
      {
         m_physicalJMSSession = m_physicalJMSConnection.createSession(false, Session.AUTO_ACKNOWLEDGE);
      }

      m_jmsTransactionContext =
         new JMSTransactionContext(m_physicalJMSSession, m_jmsEngineAdapter.isSameRMUsed());
   }

   // opearations

   public JMSEngineAdapter getJmsEngineAdapter()
   {
      return m_jmsEngineAdapter;
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#createConnection(javax.security.auth.Subject, javax.resource.spi.ConnectionRequestInfo)
    */
   protected GenericConnection createConnection(Subject subject, ConnectionRequestInfo cri) throws ResourceException
   {
      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Creating new connection handle in " + this);
      }
      
      return new JMSConnection();
   }

   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#getLocalTransaction()
    */
   public LocalTransaction getLocalTransaction() throws ResourceException
   {
      return m_jmsTransactionContext;
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#getXAResource()
    */
   public XAResource getXAResource() throws ResourceException
   {
      if (m_physicalJMSSession instanceof XASession)
      {
         return m_jmsTransactionContext;
      }

      throw new ResourceException("XA transactions not supported in " + this);
   }
   
   /**
    * @see nexj.core.rpc.ra.GenericManagedConnection#matches(javax.security.auth.Subject, nexj.core.rpc.ra.GenericConnectionRequestInfo)
    */
   public boolean matches(Subject subject, GenericConnectionRequestInfo cri)
   {
      return ObjUtil.equal(m_jmsConnectionRequestInfo, cri);
   }
   
   /**
    * @throws ResourceException 
    * @see nexj.core.rpc.ra.GenericManagedConnection#destroy()
    */
   public void destroy() throws ResourceException
   {
      try
      {
         super.destroy();
      }
      finally
      {
         if (m_physicalJMSSession != null)
         {
            try
            {
               m_physicalJMSSession.close();
            }
            catch (JMSException e)
            {
               s_logger.error("Error while closing JMS session in " + this, e);
            }
         }
         
         if (m_physicalJMSConnection != null)
         {
            try
            {
               m_physicalJMSConnection.close();
            }
            catch (JMSException e)
            {
               throw new ResourceException(e);
            }
         }
      }
   }
   
   /**
    * @param bCached True if the cached physical JMS session should be returned.
    * @param bTransacted The transacted flag.
    * @param nAcknowledgeMode The acknowledge mode.
    * @return The physical JMS session.
    * @throws JMSException
    */
   protected Session getPhysicalJMSSession(boolean bCached, boolean bTransacted, int nAcknowledgeMode) throws JMSException
   {
      if (m_jmsTransactionContext.isEnlisted())
      {
         if (bCached)
         {
            return m_jmsEngineAdapter.getSession(m_physicalJMSSession);
         }

         Session physJMSSession;

         if (m_physicalJMSConnection instanceof XAConnection)
         {
            physJMSSession = ((XAConnection)m_physicalJMSConnection).createXASession();
         }
         else
         {
            physJMSSession = m_physicalJMSConnection.createSession(bTransacted, nAcknowledgeMode);
         }

         try
         {
            m_jmsEngineAdapter.setSharedTransactions(physJMSSession, m_physicalJMSSession);
         }
         catch (Throwable t)
         {
            throw JMSResourceAdapter.createJMSException("Error creating a JMS session in " + this, t);
         }

         return m_jmsEngineAdapter.getSession(m_physicalJMSSession);
      }

      if (bCached && (m_physicalJMSConnection instanceof XAConnection || (!bTransacted && nAcknowledgeMode == Session.AUTO_ACKNOWLEDGE)))
      {
         return m_jmsEngineAdapter.getSession(m_physicalJMSSession);
      }

      if (m_physicalJMSConnection instanceof XAConnection)
      {
         return m_jmsEngineAdapter.getSession(((XAConnection)m_physicalJMSConnection).createXASession());
      }

      return  m_physicalJMSConnection.createSession(bTransacted, nAcknowledgeMode);
   }
}
