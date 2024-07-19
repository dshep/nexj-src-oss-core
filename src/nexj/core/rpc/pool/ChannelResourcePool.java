package nexj.core.rpc.pool;

import nexj.core.meta.integration.Channel;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Channel-specific resource pool.
 */
public abstract class ChannelResourcePool extends StatResourcePool
{
   // associations

   /**
    * The integration channel.
    */
   protected Channel m_channel;
   
   // constructors

   /**
    * Constructs the resource pool.
    * @param channel The integration channel. 
    */
   protected ChannelResourcePool(Channel channel)
   {
      m_channel = channel;
      m_logger = channel.getLogger();
   }

   // operations

   /**
    * @see nexj.core.util.pool.resource.GenericResourcePool#getLogger()
    */
   protected Logger getLogger()
   {
      return null;
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getStatPath()
    */
   protected String getStatPath()
   {
      return m_channel.getSenderStatPath();
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getStatClassName()
    */
   protected String getStatClassName()
   {
      return Channel.STAT_PERSIST_CLASS;
   }

   /**
    * @see nexj.core.rpc.pool.StatResourcePool#getResourceName()
    */
   protected String getResourceName()
   {
      return "Connection";
   }

   /**
    * @see nexj.core.util.pool.resource.TransactionalResourcePool#isTransactional()
    */
   public boolean isTransactional()
   {
      return m_channel.isTransactional();
   }

   /**
    * @see nexj.core.util.pool.resource.TransactionalResourcePool#isAssociated()
    */
   public boolean isAssociated()
   {
      return m_channel.isTransactional();
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + "(channel=" + m_channel.getName() + ')';
   }
}
