// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.mail.ra;

import java.util.Properties;

import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.util.ObjUtil;

/**
 * Bean used for passing Mail Session creation parameters.
 */
public class MailConnectionRequestInfo extends GenericConnectionRequestInfo
{
   // attributes

   /**
    * Mail send requires authentication against mail store.
    * @see nexj.core.meta.integration.channel.mail.Mail#OUTAUTH_INFIRST
    */
   protected boolean m_bInFirst;

   /**
    * The connection user.
    */
   protected String m_sUser;

   /**
    * The connection password.
    */
   protected String m_sPassword;

   /**
    * The connection properties.
    */
   protected Properties m_properties;

   /**
    * Precomputed cached hash code.
    */
   protected int m_nHash;

   // constructors

   /**
    * Constructs the Mail connection request info.
    * @param sUser The mail spool user.
    * @param sPassword The mail spool password.
    * @param bInFirst Require login to mail store before sending mail.
    * @param conf The mail session configuration properties.
    */
   public MailConnectionRequestInfo(
      String sUser, String sPassword, boolean bInFirst, Properties conf)
   {
      assert conf != null;

      m_sUser = sUser;
      m_sPassword = sPassword;
      m_bInFirst = bInFirst;
      m_properties = conf;
      m_nHash = m_properties.hashCode();

      if (m_sPassword != null)
      {
         m_nHash ^= m_sPassword.hashCode();
      }

      if (m_sUser != null)
      {
         m_nHash ^= m_sUser.hashCode();
      }

      m_nHash ^= (m_bInFirst) ? 1 : 0;
   }

   // operations

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof MailConnectionRequestInfo)
      {
         MailConnectionRequestInfo other = (MailConnectionRequestInfo)obj;

         return m_nHash == other.m_nHash &&
                m_bInFirst == other.m_bInFirst &&
                ObjUtil.equal(m_sUser, other.m_sUser) &&
                ObjUtil.equal(m_sPassword, other.m_sPassword) &&
                ObjUtil.equal(m_properties, other.m_properties);
      }

      return false;
   }

   /**
    * @see nexj.core.rpc.mail.ra.MailConnectionRequestInfo#m_properties
    */
   public Properties getProperties()
   {
      return m_properties;
   }

   /**
    * @see nexj.core.rpc.mail.ra.MailConnectionRequestInfo#m_sPassword
    */
   public String getPassword()
   {
      return m_sPassword;
   }

   /**
    * @see nexj.core.rpc.mail.ra.MailConnectionRequestInfo#m_sUser
    */
   public String getUser()
   {
      return m_sUser;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_nHash;
   }

   public boolean isInFirst()
   {
      return m_bInFirst;
   }
}