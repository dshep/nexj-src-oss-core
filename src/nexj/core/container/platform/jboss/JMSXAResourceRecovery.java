package nexj.core.container.platform.jboss;

import java.sql.SQLException;
import java.util.Properties;

import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameNotFoundException;
import javax.naming.NamingException;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import com.arjuna.ats.jta.recovery.XAResourceRecovery;

import nexj.core.util.BeanAccessor;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.PropertyUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * Recovery module for JMS XAResources
 */
public class JMSXAResourceRecovery implements XAResourceRecovery
{
   // attributes

   /**
    * The name of type of the JMS engine.
    */
   private String m_sJMSAdapter;

   /**
    * The name of the connection factory.
    */
   private String m_sConnectionFactoryName;

   /**
    * Indicates whether there are more resources to recover.
    */
   private boolean m_bMore;

   /**
    * Username for JMS connection.
    */
   private String m_sUser;

   /**
    * Password for JMS connection.
    */
   private String m_sPassword;

   /**
    * Time to wait after recovery start before creating a JMS connection required by some JMS Resource Adapters
    * to prevent a potential "resource not deployed" error from being thrown.
    */
   private long m_lWaitInMillis = 5000;

   // associations
   
   /**
    * The XAResource that will be submitted for recovery.
    */
   private XAResource m_xarWrapper;
   
   /**
    * The temporary XAResource - necessary, as init is called only once, so if the real XAR won't be available
    * at that point (happens for JMS native resource adapters), the recovery will always fail
    */
   private XAResource m_realXAResource;

   /**
    * This reference is required for JBossMessaging so that when this class is GC'd, finalize is called on the supplierStrategy to close the connection.
    */
   private XAResourceSupplierStrategy m_xarSupplierStrategy;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(JMSXAResourceRecovery.class);

   // attribute setters

   /**
    * Set the property JMSAdapter.
    * @param sJMSAdapter JMSAdapter name.
    */
   public void setJMSAdapter(String sJMSAdapter)
   {
      m_sJMSAdapter = sJMSAdapter;
   }

   /**
    * Set the property ConnectionFactoryName.
    * @param sConnectionFactoryName Connection Factory Name.
    */
   public void setConnectionFactoryName(String sConnectionFactoryName)
   {
      m_sConnectionFactoryName = sConnectionFactoryName;
   }

   /**
    * Set the property user.
    * @param sUser Username.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }   

   /**
    * Set the property password.
    * @param sPassword Password.
    */
   public void setPassword(String sPassword)
   {
      m_sPassword = sPassword;

      Properties properties = new Properties(SysUtil.getConfigProperties());

      try
      {
         CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

         dispatcher.init(properties);

         m_sPassword = dispatcher.decrypt(m_sPassword);
      }
      catch (Exception ex)
      {
         m_sPassword = "";
      }
   }

   /**
    * Set the property waitInMillis.
    * @param lWaitInMillis Time in milliseconds ro wait before trying to get a JMS connection.
    */
   public void setWaitInMillis(long lWaitInMillis)
   {
      m_lWaitInMillis = lWaitInMillis;
   }

   /**
    * @see com.arjuna.ats.jta.recovery.XAResourceRecovery#getXAResource()
    */
   public XAResource getXAResource() throws SQLException
   {
      return m_xarWrapper;
   }

   /**
    * @see com.arjuna.ats.jta.recovery.XAResourceRecovery#hasMoreResources()
    */
   public boolean hasMoreResources()
   {
      m_bMore = (!m_bMore);

      return m_bMore;
   }

   /**
    * @see com.arjuna.ats.jta.recovery.XAResourceRecovery#initialise(java.lang.String)
    */
   public boolean initialise(String sConfig) throws SQLException
   {
      try
      {
         Properties propertiesProvided = PropertyUtil.fromString(sConfig);
         new BeanAccessor().setProperties(this, propertiesProvided); 
      }
      catch (Throwable t)
      {
         SQLException sqle = new SQLException();

         sqle.initCause(t);

         throw sqle;
      }

      if (m_sJMSAdapter == null)
      {
         throw new SQLException("The property \"jmsAdapter\" must be provided for the configured recovery module");
      }
      
      if (m_sConnectionFactoryName == null)
      {
         throw new SQLException("The property \"connectionFactoryName\" must be provided for the configured recovery module");
      }
      
      m_xarWrapper = new DelayedInitXAResource();

      return true;
   }

   /**
    * The method to get the underlying XAResource
    * @return the underlying XAResource
    * @throws XAException
    */
   public XAResource getWrappedXAResource() throws XAException
   {
      if (m_realXAResource != null)
      {
         return m_realXAResource;
      }

      try
      {
         XAResourceSupplierStrategy xarSupplierStrategy = (XAResourceSupplierStrategy) Class.forName(m_sJMSAdapter).getConstructor(new Class[]{}).newInstance(new Object[]{});

         if (xarSupplierStrategy.isFinalizationRequired())
         {
            // Keep reference around so it is finalized only when this class is GC'd.
            m_xarSupplierStrategy = xarSupplierStrategy;
            m_realXAResource = m_xarSupplierStrategy.getXAResource(getConnectionFactory(), m_sUser, m_sPassword, m_lWaitInMillis);
         }
         else
         {
            m_realXAResource = xarSupplierStrategy.getXAResource(getConnectionFactory(), m_sUser, m_sPassword, m_lWaitInMillis);
         }
      }
      catch (NameNotFoundException e)
      {
         throw new XAException("The JMS connection factory is unavailable (this error should be ignored during JBoss startup)");
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }

      return m_realXAResource;
   }

   /**
    * Gets the connection factory from the initial context.
    * @return Connection factory from the initial context.
    * @throws NamingException
    */
   public ConnectionFactory getConnectionFactory() throws NamingException
   {
      Context ctx = new InitialContext();
      return (ConnectionFactory)ctx.lookup("java:/" + m_sConnectionFactoryName);
   }

   /**
    * Needed to delay the initialization of the real XAResource, and to avoid getting the resource not deployed error for native resource adapters. 
    */
   public class DelayedInitXAResource implements XAResource
   {
      /**
       * @see javax.transaction.xa.XAResource#commit(javax.transaction.xa.Xid, boolean)
       */
      public void commit(Xid xid, boolean bOnePhase) throws XAException
      {
         getWrappedXAResource().commit(xid, bOnePhase);
      }

      /**
       * @see javax.transaction.xa.XAResource#end(javax.transaction.xa.Xid, int)
       */
      public void end(Xid xid, int flags) throws XAException
      {
         getWrappedXAResource().end(xid, flags);
      }

      /**
       * @see javax.transaction.xa.XAResource#forget(javax.transaction.xa.Xid)
       */
      public void forget(Xid xid) throws XAException
      {
         getWrappedXAResource().forget(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#getTransactionTimeout()
       */
      public int getTransactionTimeout() throws XAException
      {
         return getWrappedXAResource().getTransactionTimeout();
      }

      /**
       * @see javax.transaction.xa.XAResource#isSameRM(javax.transaction.xa.XAResource)
       */
      public boolean isSameRM(XAResource xar) throws XAException
      {
         return getWrappedXAResource().isSameRM(xar);
      }

      /**
       * @see javax.transaction.xa.XAResource#prepare(javax.transaction.xa.Xid)
       */
      public int prepare(Xid xid) throws XAException
      {
         return getWrappedXAResource().prepare(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#recover(int)
       */
      public Xid[] recover(int flags) throws XAException
      {
         return getWrappedXAResource().recover(flags);
      }

      /**
       * @see javax.transaction.xa.XAResource#rollback(javax.transaction.xa.Xid)
       */
      public void rollback(Xid xid) throws XAException
      {
         getWrappedXAResource().rollback(xid);
      }

      /**
       * @see javax.transaction.xa.XAResource#setTransactionTimeout(int)
       */
      public boolean setTransactionTimeout(int seconds) throws XAException
      {
         return getWrappedXAResource().setTransactionTimeout(seconds);
      }

      /**
       * @see javax.transaction.xa.XAResource#start(javax.transaction.xa.Xid, int)
       */
      public void start(Xid xid, int flags) throws XAException
      {
         getWrappedXAResource().start(xid, flags); 
      }
   }

   /**
    * The interface of XAResource supplier strategy
    */
   public interface XAResourceSupplierStrategy
   {
      /**
       * Fetches XAResource for recovery.
       * @param connectionFactory Connection factory that will be used to create JMS connection and session and get XAResource.
       * @param sUser Username.
       * @param sPassword Password.
       * @param lWainInMillis Time to wait before creating jms connection.
       * @return XAResource for recovery.
       * @throws JMSException
       */
      public XAResource getXAResource(ConnectionFactory connectionFactory, String sUser, String sPassword, long lWainInMillis) throws JMSException;

      /**
       * Necessary when the JMS connection should stay open during the recovery as otherwise the methods
       * on its XAResource will fail. In this case we should close the connection at finalize ().
       * @return True if finalization is necessary, false - otherwise.
       */
      public boolean isFinalizationRequired();
   }
}
