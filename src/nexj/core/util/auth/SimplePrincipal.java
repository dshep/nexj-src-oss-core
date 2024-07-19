// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util.auth;

import java.io.Serializable;
import java.security.Principal;

/**
 * Principal containing a user name.
 */
public class SimplePrincipal implements Principal, Serializable
{
   // constants
   
   /**
    * Serialization UID.
    */
   private final static long serialVersionUID = 2398772264792614539L;

   // attributes
   
   /**
    * The user name.
    */
   protected String m_sName;
   
   // constructors
   
   /**
    * Costructs the principal.
    * @param sName The user name.
    */
   public SimplePrincipal(String sName)
   {
      m_sName = sName;
   }
   
   // operations
   
   /**
    * @see java.security.Principal#getName()
    */
   public String getName()
   {
      return m_sName;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return m_sName;
   }
}
