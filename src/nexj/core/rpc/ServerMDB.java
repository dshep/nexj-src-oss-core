// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import javax.ejb.EJBException;
import javax.ejb.MessageDrivenBean;
import javax.ejb.MessageDrivenContext;

import nexj.core.runtime.LifecycleManager;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Generic server MDB implementation.
 */
public abstract class ServerMDB implements MessageDrivenBean
{
   // constants
   
   /**
    * Serialization version. 
    */
   private final static long serialVersionUID = 892036288893734917L;

   // associations

   /**
    * The MDB context.
    */
   protected MessageDrivenContext m_messageContext;

   // operations

   /**
    * @return The class logger.
    */
   protected abstract Logger getLogger();

   /**
    * Initializes the MDB.
    */
   protected void init() throws Exception
   {
   }

   /**
    * Initializes the bean.
    */
   public void ejbCreate()
   {
      try
      {
         LifecycleManager.getLifecycle().startup();

         init();

         if (getLogger().isDebugEnabled())
         {
            getLogger().debug("Created " + this);
         }
      }
      catch (Exception e)
      {
         String sMsg = "Unable to create " + this;
         
         getLogger().error(sMsg, e);

         try
         {
            // Slow down errors
            synchronized (this)
            {
               wait(3000);
            }
         }
         catch (Throwable t)
         {
         }

         throw new EJBException(sMsg, e);
      }
   }

   /**
    * @see javax.ejb.MessageDrivenBean#ejbRemove()
    */
   public void ejbRemove() throws EJBException
   {
      if (getLogger().isDebugEnabled())
      {
         getLogger().debug("Removed " + this);
      }
   }

   /**
    * @see javax.ejb.MessageDrivenBean#setMessageDrivenContext(javax.ejb.MessageDrivenContext)
    */
   public void setMessageDrivenContext(MessageDrivenContext ctx) throws EJBException
   {
      m_messageContext = ctx;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append(ObjUtil.getShortClassName(this)); 
      buf.append('@');
      buf.append(System.identityHashCode(this));

      return buf.toString();
   }
}
