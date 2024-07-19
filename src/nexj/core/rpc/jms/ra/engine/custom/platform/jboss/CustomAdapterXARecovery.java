package nexj.core.rpc.jms.ra.engine.custom.platform.jboss;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.XASession;
import javax.transaction.xa.XAResource;

import nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy;
import nexj.core.util.ObjUtil;

/**
 * An implementation of the of XAResource supplier strategy for our engine adapters
 */
public class CustomAdapterXARecovery implements XAResourceSupplierStrategy
{
   /**
    * @see nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy#getXAResource(java.lang.String, java.lang.String, java.lang.String, long)
    */
   public XAResource getXAResource(ConnectionFactory connectionFactory, String sUser, String sPassword, long lWainInMillis) throws JMSException
   {
      XAResource xar = null;
      Connection conn = null;

      try
      {
         if (sUser == null)
         {
            conn = connectionFactory.createConnection();
         }
         else
         {
            conn = connectionFactory.createConnection(sUser, sPassword);
         }

         Object session = Class.forName("nexj.core.rpc.jms.ra.JMSConnection", true, conn.getClass().getClassLoader()).getMethod("getCachedPhysicalJMSSession", null).invoke(conn, null);

         xar = ((XASession)session).getXAResource();
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
      finally
      {
         if (conn != null)
         {
            conn.close();
         }
      }

      return xar;
   }

   /**
    * @see nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy#isFinalizationRequired()
    */
   public boolean isFinalizationRequired()
   {
      return false;
   }
}
