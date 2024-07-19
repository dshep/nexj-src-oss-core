package nexj.core.rpc.jms.ra.platform.activemq.platform.jboss;

import java.lang.reflect.Field;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.transaction.xa.XAResource;

import nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy;
import nexj.core.util.ObjUtil;

/**
 * An implementation of the of XAResource supplier strategy for ActiveMQ native resource adapter
 */
public class ActiveMQPlatformAdapterXARecovery implements XAResourceSupplierStrategy
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

         Field managedConnectionField = conn.getClass().getDeclaredField("managedConnection");

         managedConnectionField.setAccessible(true);

         Object managedConnection = managedConnectionField.get(conn);
         Field transactionContextField = managedConnection.getClass().getDeclaredField("transactionContext");

         transactionContextField.setAccessible(true);

         Object transactionContext = transactionContextField.get(managedConnection);
         
         if (!(transactionContext instanceof XAResource))
         {
            return null;
         }

         xar = (XAResource)transactionContext;
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

