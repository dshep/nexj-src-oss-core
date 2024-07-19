// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import javax.ejb.EJBException;
import javax.naming.InitialContext;

import nexj.core.util.J2EEUtil;


/**
 * Generic channel-aware MDB implementation.
 */
public abstract class IntegrationMDB extends ServerMDB
{
   // constants
   
   /**
    * Serialization version. 
    */
   private final static long serialVersionUID = -4980673887386132841L;

   // attributes

   /**
    * The integration channel name.
    */
   protected String m_sChannelName;

   // operations

   /**
    * @see nexj.core.rpc.ServerMDB#init()
    */
   protected void init() throws Exception
   {
      m_sChannelName = (String)new InitialContext().lookup(J2EEUtil.JNDI_ENV_PREFIX + "channel");
   }

   /**
    * @see nexj.core.rpc.ServerMDB#ejbRemove()
    */
   public void ejbRemove() throws EJBException
   {
      super.ejbRemove();
      m_sChannelName = null;
   }

   /**
    * @see nexj.core.rpc.ServerMDB#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(super.toString());

      if (m_sChannelName != null)
      {
         buf.append("(channel=");
         buf.append(m_sChannelName);
         buf.append(')');
      }

      return buf.toString();
   }
}
