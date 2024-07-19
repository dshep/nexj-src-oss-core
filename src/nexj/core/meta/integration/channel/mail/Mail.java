// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.integration.channel.mail;

import nexj.core.meta.MetadataException;
import nexj.core.meta.PropertyHolder;
import nexj.core.meta.integration.Channel;
import nexj.core.meta.integration.channel.jms.MessageQueue;

/**
 * Mail server metadata.
 */
public class Mail extends Channel
{
   // attributes

   /**
    * No Encryption for mail channel.
    */
   protected final static byte ENCRYPTION_NONE = 0;

   /**
    * SSL Encryption for mail channel.
    */
   protected final static byte ENCRYPTION_SSL = 1;

   /**
    * TLS Encryption for mail channel.
    */
   protected final static byte ENCRYPTION_TLS = 2;

   /**
    * No authentication for outgoing mail.
    */
   protected final static byte OUTAUTH_NONE = 0;

   /**
    * Open a connection to the mail store before sending mail.
    */
   protected final static byte OUTAUTH_INFIRST = 1;

   /**
    * Use user/password authentication credentials when sending mail.
    */
   protected final static byte OUTAUTH_CREDENTIAL = 2;

   /**
    * The JNDI alias.
    */
   protected String m_sAlias;

   /**
    * The default user account for processing the messages.
    */
   protected String m_sDefaultUser;

   /**
    * The default sender mail address.
    */
   protected String m_sFrom;

   /**
    * The mail user.
    */
   protected String m_sUser;

   /**
    * The mail password.
    */
   protected String m_sPassword = "";

   /**
    * The read protocol.
    */
   protected String m_sInProtocol = "pop3";

   /**
    * The mail server incoming folder.
    */
   protected String m_sInFolder = "INBOX";

   /**
    * The incoming host.
    */
   protected String m_sInHost;

   /**
    * The outgoing host.
    */
   protected String m_sOutHost;

   /**
    * The outgoing protocol.
    */
   protected String m_sOutProtocol = "smtp";

   /**
    * The type of encryption to apply for inbound mail channel.
    */
   protected byte m_nInEncryption = ENCRYPTION_SSL;

   /**
    * The incoming port.
    */
   protected int m_nInPort = -1;

   /**
    * Type of authentication to use with SMTP prior to sending mail, one of the OUTAUTH_* constants.
    */
   protected byte m_nOutAuth = OUTAUTH_NONE;

   /**
    * The outgoing port.
    */
   protected int m_nOutPort = -1;

   /**
    * The type of encryption to apply for outbound mail channel.
    */
   protected byte m_nOutEncryption = ENCRYPTION_NONE;

   /**
    * True if this mail channel is the first with a given alias.
    */
   protected boolean m_bFirst = true;

   // associations
   
   /**
    * The connection properties.
    */
   protected PropertyHolder m_props = new PropertyHolder();

   /**
    * The optional queue for forwarding the received messages.
    */
   protected MessageQueue m_queue;

   // constructors

   /**
    * Constructs the mail data source.
    * @param sName The data source name.
    */
   public Mail(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.integration.Channel#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();
      m_props.makeReadOnly();
   }

   /**
    * @see nexj.core.meta.integration.Channel#isTransactional()
    */
   public boolean isTransactional()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.integration.Channel#isSynchronous()
    */
   public boolean isSynchronous()
   {
      return false;
   }

   /**
    * Sets the JNDI alias.
    * @param sAlias The JNDI alias to set.
    */
   public void setAlias(String sAlias)
   {
      verifyNotReadOnly();
      m_sAlias = sAlias;
   }

   /**
    * @return The JNDI alias.
    */
   public String getAlias()
   {
      return m_sAlias;
   }

   /**
    * Sets the default user account for processing the messages.
    * @param sDefaultUser The default user account for processing the messages to set.
    */
   public void setDefaultUser(String sDefaultUser)
   {
      verifyNotReadOnly();
      m_sDefaultUser = sDefaultUser;
   }

   /**
    * @return The default user account for processing the messages.
    */
   public String getDefaultUser()
   {
      return m_sDefaultUser;
   }

   /**
    * Sets the mail user.
    * @param sUser The mail user to set.
    */
   public void setUser(String sUser)
   {
      verifyNotReadOnly();
      m_sUser = sUser;
   }

   /**
    * @return The mail user.
    */
   public String getUser()
   {
      return m_sUser;
   }
   
   /**
    * Sets the mail password.
    * @param sPassword The mail password to set.
    */
   public void setPassword(String sPassword)
   {
      verifyNotReadOnly();
      m_sPassword = sPassword;
   }

   /**
    * @return The mail password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nInEncryption
    */
   public void setInEncryption(byte nEncryption)
   {
      assert nEncryption == ENCRYPTION_NONE ||
             nEncryption == ENCRYPTION_SSL ||
             nEncryption == ENCRYPTION_TLS;

      verifyNotReadOnly();
      m_nInEncryption = nEncryption;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nInEncryption
    */
   public byte getInEncryption()
   {
      return m_nInEncryption;
   }

   /**
    * Sets the incoming protocol.
    * @param sInProtocol The incoming protocol to set.
    */
   public void setInProtocol(String sInProtocol)
   {
      verifyNotReadOnly();
      m_sInProtocol = sInProtocol;
   }

   /**
    * @return The incoming protocol.
    */
   public String getInProtocol()
   {
      return m_sInProtocol;
   }

   /**
    * Sets the incoming host.
    * @param sInHost The incoming host to set.
    */
   public void setInHost(String sInHost)
   {
      verifyNotReadOnly();
      m_sInHost = sInHost;
   }

   /**
    * @return The incoming host.
    */
   public String getInHost()
   {
      return m_sInHost;
   }
   
   /**
    * Sets the incoming port.
    * @param nInPort The incoming port to set.
    */
   public void setInPort(int nInPort)
   {
      verifyNotReadOnly();
      m_nInPort = nInPort;
   }

   /**
    * @return The incoming port.
    */
   public int getInPort()
   {
      return m_nInPort;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nOutAuth
    */
   public void setOutAuth(byte nAuth)
   {
      assert nAuth == OUTAUTH_NONE || nAuth == OUTAUTH_INFIRST || nAuth == OUTAUTH_CREDENTIAL;

      verifyNotReadOnly();
      m_nOutAuth = nAuth;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nOutAuth
    */
   public byte getOutAuth()
   {
      return m_nOutAuth;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nOutEncryption
    */
   public void setOutEncryption(byte nEncryption)
   {
      assert nEncryption == ENCRYPTION_NONE ||
             nEncryption == ENCRYPTION_SSL ||
             nEncryption == ENCRYPTION_TLS;

      verifyNotReadOnly();
      m_nOutEncryption = nEncryption;
   }

   /**
    * @see nexj.core.meta.integration.channel.mail.Mail#m_nOutEncryption
    */
   public byte getOutEncryption()
   {
      return m_nOutEncryption;
   }

   /**
    * Sets the write protocol.
    * @param sOutProtocol The write protocol to set.
    */
   public void setOutProtocol(String sOutProtocol)
   {
      verifyNotReadOnly();
      m_sOutProtocol = sOutProtocol;
   }

   /**
    * @return The write protocol.
    */
   public String getOutProtocol()
   {
      return m_sOutProtocol;
   }

   /**
    * Sets the write host.
    * @param sOutHost The write host to set.
    */
   public void setOutHost(String sOutHost)
   {
      verifyNotReadOnly();
      m_sOutHost = sOutHost;
   }

   /**
    * @return The write host.
    */
   public String getOutHost()
   {
      return m_sOutHost;
   }

   /**
    * Sets the write port.
    * @param nOutPort The write port to set.
    */
   public void setOutPort(int nOutPort)
   {
      verifyNotReadOnly();
      m_nOutPort = nOutPort;
   }

   /**
    * @return The write port.
    */
   public int getOutPort()
   {
      return m_nOutPort;
   }
   
   /**
    * Sets the mail server incoming folder.
    * @param sFolder The mail server incoming folder to set.
    */
   public void setInFolder(String sFolder)
   {
      verifyNotReadOnly();
      m_sInFolder = sFolder;
   }

   /**
    * @return The mail server incoming folder.
    */
   public String getInFolder()
   {
      return m_sInFolder;
   }
   
   /**
    * Sets the default sender mail address.
    * @param sFrom The default sender mail address to set.
    */
   public void setFrom(String sFrom)
   {
      verifyNotReadOnly();
      m_sFrom = sFrom;
   }

   /**
    * @return The default sender mail address.
    */
   public String getFrom()
   {
      return m_sFrom;
   }

   /**
    * Sets whether or not this mail channel is the first with a given alias.
    * @param bFirst True if this mail channel is the first with a given alias.
    */
   public void setFirst(boolean bFirst)
   {
      verifyNotReadOnly();
      m_bFirst = bFirst;
   }

   /**
    * Gets whether or not this mail channel is the first with a given alias.
    * @return True if this mail channel is the first with a given alias.
    */
   public boolean isFirst()
   {
      return m_bFirst;
   }

   /**
    * @return The connection properties.
    */
   public PropertyHolder getPropertyHolder()
   {
      return m_props;
   }
   
   /**
    * Sets the default connection properties.
    */
   public void setDefaultProperties()
   {
      verifyNotReadOnly();
      
      if (m_sUser != null)
      {
         m_props.addProperty("mail.user", m_sUser);
      }
      
      if (m_sFrom != null)
      {
         m_props.addProperty("mail.from", m_sFrom);
      }

      if (m_bReceivable)
      {
         String sProtocol =
            setProtocolProperties(m_props, m_sInProtocol, m_nInEncryption, m_sInHost, m_nInPort);

         m_props.addProperty("mail.store.protocol", sProtocol);
      }

      if (m_bSendable)
      {
         String sProtocol =
            setProtocolProperties(m_props,
                                  m_sOutProtocol, m_nOutEncryption, m_sOutHost, m_nOutPort);

         m_props.addProperty("mail.transport.protocol", sProtocol);

         if (m_nOutAuth == OUTAUTH_CREDENTIAL)
         {
            m_props.addProperty("mail." + sProtocol + ".auth", "true");

            if (sProtocol.endsWith("s")) // strip the terminating 's' from the SSL enabled protocol
            {
               m_props.addProperty(
                  "mail." + sProtocol.substring(0, sProtocol.length() - 1) + ".auth", "true");
            }
         }
      }
   }

   /**
    * Set all the common protocol related properties.
    * @param props The properties object to modify.
    * @param sProtocol The protocol to set properties for.
    * @param nEncryption The type of encryption the protocol should use.
    * @param sHost The host the protocol should connect to.
    * @param nPort The port the protocol should connect to.
    * @return The actual protocol that should be used by javamail.
    */
   protected static String setProtocolProperties(
      PropertyHolder props, String sProtocol, byte nEncryption, String sHost, int nPort)
   {
      if (sProtocol.endsWith("s")) // works for: pop3s/imaps/nntps/smtps
      {
         sProtocol = sProtocol.substring(0, sProtocol.length() - 1);//strip 's' to get base protocol

         if (nEncryption == ENCRYPTION_NONE)
         {
            nEncryption = ENCRYPTION_SSL; // inherit encryption from SSL protocol
         }
      }

      String sPrefix = "mail." + sProtocol;

      props.addProperty(sPrefix + ".host", sHost);

      if (nPort >= 0)
      {
         props.addProperty(sPrefix + ".port", String.valueOf(nPort));
      }

      if (nEncryption == ENCRYPTION_SSL)
      {
         // Specification says to use protocol name in prefix, however in actual fact the non-SSL
         // version of the protocol seems to be checked. Hence set both to comply with spec.
         String sPrefixSSL = sPrefix + 's';

         props.addProperty(sPrefixSSL + ".host", sHost);

         if (nPort >= 0)
         {
            props.addProperty(sPrefixSSL + ".port", String.valueOf(nPort));
         }

         props.addDefaultProperty(sPrefix + ".ssl.enable", "true");
         props.addDefaultProperty(sPrefixSSL + ".ssl.enable", "true");

         props.addDefaultProperty(
            sPrefix + ".socketFactory.class", "javax.net.ssl.SSLSocketFactory");
         props.addDefaultProperty(
            sPrefixSSL + ".socketFactory.class", "javax.net.ssl.SSLSocketFactory");

         props.addDefaultProperty(sPrefix + ".socketFactory.fallback", "false");
         props.addDefaultProperty(sPrefixSSL + ".socketFactory.fallback", "false");

         if (nPort >= 0)
         {
            props.addDefaultProperty(sPrefix + ".socketFactory.port", String.valueOf(nPort));
            props.addDefaultProperty(sPrefixSSL + ".socketFactory.port", String.valueOf(nPort));
         }

         sProtocol += 's'; // the SSL enabled version of the protocol
      }
      else if (nEncryption == ENCRYPTION_TLS)
      {
         props.addDefaultProperty(sPrefix + ".starttls.enable", "true");
         props.addDefaultProperty(sPrefix + ".starttls.required", "true");
      }

      return sProtocol;
   }

   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue The optional queue for forwarding the received messages to set.
    */
   public void setQueue(Channel queue)
   {
      verifyNotReadOnly();

      if (queue != null && !(queue instanceof MessageQueue))
      {
         throw new MetadataException("err.meta.integration.mail.queueChannel",
                                     new Object[]{getName()});
      }

      setQueue((MessageQueue)queue);
   }

   /**
    * Sets the optional queue for forwarding the received messages.
    * @param queue The optional queue for forwarding the received messages to set.
    */
   public void setQueue(MessageQueue queue)
   {
      verifyNotReadOnly();

      if (queue != null)
      {
         if (!m_bReceivable)
         {
            throw new MetadataException("err.meta.integration.mail.queue",
                                        new Object[]{queue.getName(), getName()});
         }

         if (!queue.isSendable())
         {
            throw new MetadataException("err.meta.integration.mail.nonSenderQueue",
                                        new Object[]{queue.getName(), getName()});
         }
      }

      m_queue = queue;
   }

   /**
    * @return The optional queue for forwarding the received messages.
    */
   public MessageQueue getQueue()
   {
      return m_queue;
   }
}