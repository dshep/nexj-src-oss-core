// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.jms.ra;

import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.util.ObjUtil;

/**
 * Bean used for passing JMS connection creation parameters.
 */
public class JMSConnectionRequestInfo extends GenericConnectionRequestInfo
{
   // attributes

   /**
    * The JMS connection user.
    */
   protected String m_sUserName;

   /**
    * The JMS connection password.
    */
   protected String m_sPassword;
   
   // constructors
   
   /**
    * Constructs an JMS connection request info.
    * @param sUserName The JMS connection user
    * @param sPassword The JMS connection password
    */
   public JMSConnectionRequestInfo(String sUserName, String sPassword)
   {
      m_sUserName = sUserName;
      m_sPassword = sPassword;
   }
   
   // opearations

   /**
    * Sets the JMS connection user.
    * @param sUserName The JMS connection user to set.
    */
   public void setUserName(String sUserName)
   {
      m_sUserName = sUserName;
   }

   /**
    * @return The JMS connection user.
    */
   public String getUserName()
   {
      return m_sUserName;
   }

   /**
    * Sets the JMS connection password.
    * @param sPassword The JMS connection password to set.
    */
   public void setPassword(String sPassword)
   {
      m_sPassword = sPassword;
   }

   /**
    * @return The JMS connection password.
    */
   public String getPassword()
   {
      return m_sPassword;
   }
   
   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof JMSConnectionRequestInfo)
      {
         JMSConnectionRequestInfo other = (JMSConnectionRequestInfo)obj;

         return ObjUtil.equal(m_sPassword, other.m_sPassword) &&
                ObjUtil.equal(m_sUserName, other.m_sUserName);
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return ((m_sPassword == null) ? 0 : m_sPassword.hashCode()) ^
            ((m_sUserName == null) ? 0 : m_sUserName.hashCode());
   }
}
