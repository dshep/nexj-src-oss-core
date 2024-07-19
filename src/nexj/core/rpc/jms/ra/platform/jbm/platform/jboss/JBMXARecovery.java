package nexj.core.rpc.jms.ra.platform.jbm.platform.jboss;

import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.XAConnection;
import javax.jms.XAConnectionFactory;
import javax.jms.XASession;
import javax.transaction.xa.XAResource;

import nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy;
import nexj.core.util.ObjUtil;

/**
 * An implementation of the of XAResource supplier strategy for JBoss Messaging
 */
public class JBMXARecovery implements XAResourceSupplierStrategy
{
   protected XAConnection m_conn = null;

   /**
    * @see nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy#getXAResource(java.lang.String, java.lang.String, java.lang.String, long)
    */
   public XAResource getXAResource(ConnectionFactory connectionFactory, String sUser, String sPassword, long lWainInMillis) throws JMSException
   {
      XAResource xar = null;

      try
      {
         if (sUser == null)
         {
            m_conn = ((XAConnectionFactory)connectionFactory).createXAConnection();
         }
         else
         {
            m_conn = ((XAConnectionFactory)connectionFactory).createXAConnection(sUser, sPassword);
         }

         XASession session = m_conn.createXASession();

         xar = session.getXAResource();
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }

      return xar;
   }

   /**
    * @see nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy#isFinalizationRequired()
    */
   public boolean isFinalizationRequired()
   {
      return true;
   }

   /**
    * @see java.lang.Object#finalize()
    */
   protected void finalize() throws Throwable
   {
      if (m_conn != null)
      {
         m_conn.close();
      }
   }
}
