// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.sql.ra;

import nexj.core.rpc.ra.GenericConnectionRequestInfo;
import nexj.core.util.ObjUtil;

/**
 * Bean used for passing SQL connection creation parameters.
 */
public class SQLConnectionRequestInfo extends GenericConnectionRequestInfo
{
   // attributes

   /**
    * Database user.
    */
   protected String m_sUser;

   /**
    * Database password.
    */
   protected String m_sPassword;

   // constructors

   /**
    * Constructs an SQL connection request info with null user and password.
    */
   public SQLConnectionRequestInfo()
   {
   }

   /**
    * Constructs an SQL connection request info.
    * @param sUser The database user.
    * @param sPassword The database password.
    */
   public SQLConnectionRequestInfo(String sUser, String sPassword)
   {
      m_sUser = sUser;
      m_sPassword = sPassword;
   }

   // operations

   /**
    * Sets the database user.
    * @param sUser The database user to set.
    */
   public void setUser(String sUser)
   {
      m_sUser = sUser;
   }

   /**
    * @return The database user.
    */
   public String getUser()
   {
       return m_sUser;
   }

   /**
    * Sets the database password.
    * @param sPassword The database password to set.
    */
   public void setPassword(String sPassword)
   {
      m_sPassword = sPassword;
   }

   /**
    * @return The database password.
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
      if (obj instanceof SQLConnectionRequestInfo)
      {
         SQLConnectionRequestInfo other = (SQLConnectionRequestInfo)obj;

         return ObjUtil.equal(m_sPassword, other.m_sPassword) &&
                ObjUtil.equal(m_sUser, other.m_sUser);
      }

      return false;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return ((m_sPassword == null) ? 0 : m_sPassword.hashCode()) ^
            ((m_sUser == null) ? 0 : m_sUser.hashCode());
   }
}