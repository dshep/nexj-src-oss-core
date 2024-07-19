// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail;

import java.util.Properties;

import javax.naming.InitialContext;
import javax.resource.ResourceException;
import javax.resource.spi.ManagedConnectionFactory;

import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Initializable;
import nexj.core.util.J2EEUtil;
import nexj.core.util.Logger;
import nexj.core.util.PropertyIterator;
import nexj.core.util.SysUtil;

/**
 * A factory locator for Mail connection factories via either JNDI lookup or direct RA instantiation.
 */
public class MailConnectionFactoryLocator implements Initializable
{
   // attributes

   /**
    * The default ManagedConnectionFactory class used when run outside a container.
    */
   protected final static String MCF_CLASS =
      SysUtil.PACKAGE + ".core.rpc.mail.ra.MailManagedConnectionFactory";

   /**
    * Mail send requires authentication against mail store.
    * @see nexj.core.meta.integration.channel.mail.Mail#OUTAUTH_INFIRST
    */
   protected boolean m_bInFirst;

   // associations

   /**
    * The default properties to configure javamail connections with.
    */
   protected Properties m_conf;

   /**
    * The JNDI DataSource session name.
    */
   protected String m_sDataSource;

   /**
    * The default password to use for javamail connections.
    */
   protected String m_sPassword;

   /**
    * The default user to use for javamail connections.
    */
   protected String m_sUser;

   /**
    * The factory to request new Mail connections from.
    */
   protected MailConnectionFactory m_factory;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(MailConnectionFactoryLocator.class);

   // operations

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (!J2EEUtil.isContained()) // direct factory RA instantiation
      {
         ManagedConnectionFactory mcf =
         (ManagedConnectionFactory)Class.forName(MCF_CLASS).newInstance();

         m_factory = (MailConnectionFactory)(mcf.createConnectionFactory());

         return;
      }

      // below here JNDI lookup of factory
      assert m_sDataSource != null;

      if (m_sDataSource.indexOf(':') < 0)
      {
         if (m_sDataSource.length() > 0 && m_sDataSource.charAt(0) == '/')
         {
            m_sDataSource = m_sDataSource.substring(1);
         }
         else
         {
            m_sDataSource = J2EEUtil.JNDI_ENV_PREFIX + "mail/" + m_sDataSource;
         }
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Binding to data source \"" + m_sDataSource + "\"");
      }

      m_factory = (MailConnectionFactory)new InitialContext().lookup(m_sDataSource);
   }

   /**
    * Open a mail connection to send the specific TransferObject.
    * @param tobj The object to open the connection for (null == use default config)
    * @return A Mail connection.
    * @throws ResourceException On Mail connection open failure.
    */
   public MailConnection openConnection(TransferObject tobj) throws ResourceException
   {
      Properties conf = m_conf;
      String sPassword = m_sPassword;
      String sUser = m_sUser;

      if (tobj != null)
      {
         conf = override(conf, (TransferObject)tobj.findValue(Mail.CONFIG));
         sPassword = override(sPassword, (String)tobj.findValue(Mail.PASSWORD));
         sUser = override(sUser, (String)tobj.findValue(Mail.USER));
      }

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Starting a mail session, properties=" + conf);
      }

      return m_factory.open(conf, sUser, sPassword, m_bInFirst);
   }

   /**
    * Take all properties from "overrides" and override any found in "properties".
    * @param properties The properties to append/override (not modified).
    * @param overrides The TransferObject to get property overrides from.
    * @return The overlay of properties from "overrides" over "properties".
    */
   protected static Properties override(Properties properties, TransferObject overrides)
   {
      if (overrides != null && overrides.getValueCount() > 0)
      {
         // do not use Properties(Properties) since Properties.equals() does not consider "defaults"
         properties = (properties == null) ? new Properties() : (Properties)properties.clone();

         for (PropertyIterator itr = overrides.getIterator(); itr.hasNext();)
         {
            itr.next();
            properties.setProperty(itr.getName(), (String)itr.getValue());
         }
      }

      return properties;
   }

   /**
    * Override the default if sOverride != null.
    * @param sDefault The default value to override.
    * @param sOverride The value to override with.
    */
   protected static String override(String sDefault, String sOverride)
   {
      return (sOverride == null) ? sDefault : sOverride;
   }

   /**
    * Set the name of the JNDI data source to look up.
    * @param sDataSource The JNDI data source name.
    */
   public void setDataSource(String sDataSource)
   {
      m_sDataSource = sDataSource;
   }

   /**
    * @see nexj.core.rpc.mail.MailConnectionFactoryLocator#m_bInFirst
    */
   public void setInFirst(boolean bInFirst)
   {
      m_bInFirst = bInFirst;
   }

   /**
    * Set the default password to use with the default user for configuring javamail connections.
    * @param sPassword The default password to use with the default user for configuring javamail
    *                  connections.
    */
   public void setPassword(String sPassword)
   {
      m_sPassword = sPassword;
   }

   /**
    * Set the default properties to configure javamail connections with.
    * @param properties The default properties to configure javamail connections with.
    */
   public void setProperties(Properties conf)
   {
      m_conf = conf;
   }

   /**
    * Set the default user to use for configuring javamail connections.
    * @param sUser The default user to use for configuring javamail connections.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }

   /**
    * Convert a TransferObject to a Properties object. If already converted then just return it.
    * @param tobj The TransferObject to convert to Properties.
    * @return The 'obj' as Properties.
    */
   protected static Properties toProperties(TransferObject tobj)
   {
      Properties props = null;

      if (tobj != null)
      {
         props = new Properties();

         for (PropertyIterator itr = tobj.getIterator(); itr.hasNext();)
         {
            itr.next();
            props.setProperty(itr.getName(), (String)itr.getValue());
         }
      }

      return props;
   }
}