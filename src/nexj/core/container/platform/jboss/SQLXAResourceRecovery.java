package nexj.core.container.platform.jboss;

import java.sql.SQLException;
import java.util.Properties;

import com.arjuna.ats.internal.jbossatx.jta.AppServerJDBCXARecovery;

import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.cipher.CharacterStreamCipherDispatcher;

/**
 * The wrapper around JBoss's JDBC recovery module,
 * needed to avoid specifying open passwords in jbossts-properties.xml.
 */
public class SQLXAResourceRecovery extends AppServerJDBCXARecovery
{
   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SQLXAResourceRecovery.class);

   // constructors

   /**
    * The constructor
    * @throws SQLException
    */
   public SQLXAResourceRecovery() throws SQLException
   {
      super();
   }

   // operations

   /**
    * Decrypt the password
    */
   protected void decryptPassword()
   {
      Properties properties = new Properties(SysUtil.getConfigProperties());
      String sPassword = getDBPassword();

      if (sPassword != null)
      {
         CharacterStreamCipherDispatcher dispatcher = new CharacterStreamCipherDispatcher();

         dispatcher.init(properties);
         setDBPassword(dispatcher.decrypt(sPassword));
      }
   }

   /**
    * @see com.arjuna.ats.jta.recovery.XAResourceRecovery#initialise(java.lang.String)
    */
   public boolean initialise(String sParameter) throws SQLException
   {      
      boolean bInitSuccess = super.initialise(sParameter);

      try
      {
         decryptPassword();
      }
      catch (Throwable t)
      {
         s_logger.error("Unable to decrypt password for recovery module " + sParameter, t);
         ObjUtil.rethrow(t);
      }

      return bInitSuccess;
   }
}
