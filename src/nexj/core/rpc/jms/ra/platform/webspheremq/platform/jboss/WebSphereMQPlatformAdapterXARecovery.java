package nexj.core.rpc.jms.ra.platform.webspheremq.platform.jboss;

import java.lang.reflect.Field;
import java.util.List;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.XASession;
import javax.transaction.xa.XAResource;

import nexj.core.container.platform.jboss.JMSXAResourceRecovery.XAResourceSupplierStrategy;
import nexj.core.util.ObjUtil;

/**
 * An implementation of the of XAResource supplier strategy for WebSphere MQ native resource adapter
 */
public class WebSphereMQPlatformAdapterXARecovery implements XAResourceSupplierStrategy
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
         // Sleeping here as otherwise we will get an error due to the resource is not deployed yet
         Thread.sleep(lWainInMillis);

         if (sUser == null)
         {
            conn = connectionFactory.createConnection();
         }
         else
         {
            conn = connectionFactory.createConnection(sUser, sPassword);
         }

         Field theConnectionField = conn.getClass().getDeclaredField("theConnection");

         theConnectionField.setAccessible(true);

         Object theConnection = theConnectionField.get(conn);
         Field commonConnField = theConnection.getClass().getSuperclass().getDeclaredField("commonConn");

         commonConnField.setAccessible(true);

         Object commonConn = commonConnField.get(theConnection);
         Field sessionsField = commonConn.getClass().getSuperclass().getDeclaredField("sessions");

         sessionsField.setAccessible(true);

         Object sessions = sessionsField.get(commonConn);
         List alSessionsList = (List)sessions;

         if (alSessionsList.size() <= 0)
         {
            return null;
         }

         Session ses = (Session)alSessionsList.get(0);

         if (ses instanceof XASession)
         {
            xar = ((XASession)ses).getXAResource();
         }
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

