// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.InvalidPropertyException;
import javax.resource.spi.ResourceAdapter;

import nexj.core.util.ObjUtil;

/**
 * Generic consumer endpoint configuration, corresponding to one MDB metadata spec.
 */
public class GenericConsumerConfig implements ActivationSpec
{
   // attributes

   /**
    * The maximum consumer pool size (-1 for unlimited).
    */
   protected int m_nMaxPoolSize = 16;

   /**
    * The reconnection retry delay in milliseconds.
    */
   protected long m_lReconnectionDelay = 8000;

   /**
    * The associated channel name.
    */
   protected String m_sChannel;

   // associations

   /**
    * The resource adapter.
    */
   protected ResourceAdapter m_adapter;

   // operations

   /**
    * @see javax.resource.spi.ResourceAdapterAssociation#setResourceAdapter(javax.resource.spi.ResourceAdapter)
    */
   public void setResourceAdapter(ResourceAdapter adapter) throws ResourceException
   {
      m_adapter = adapter;
   }

   /**
    * @see javax.resource.spi.ResourceAdapterAssociation#getResourceAdapter()
    */
   public ResourceAdapter getResourceAdapter()
   {
      return m_adapter;
   }

   /**
    * Sets the maximum consumer pool size.
    * @param nMaxPoolSize The maximum consumer pool size to set.
    */
   public void setMaxPoolSize(int nMaxPoolSize)
   {
      m_nMaxPoolSize = nMaxPoolSize;
   }

   /**
    * @return The maximum consumer pool size.
    */
   public int getMaxPoolSize()
   {
      return m_nMaxPoolSize;
   }

   /**
    * Sets the reconnection retry delay in milliseconds.
    * @param lReconnectionDelay The reconnection retry delay in milliseconds to set.
    */
   public void setReconnectionDelay(long lReconnectionDelay)
   {
      m_lReconnectionDelay = lReconnectionDelay;
   }

   /**
    * @return The reconnection retry delay in milliseconds.
    */
   public long getReconnectionDelay()
   {
      return m_lReconnectionDelay;
   }

   /**
    * @return The associated channel name.
    */
   public String getChannel()
   {
      return m_sChannel;
   }

   /**
    * @param sChannel The associated channel name.
    */
   public void setChannel(String sChannel)
   {
      m_sChannel = sChannel;
   }

   /**
    * @return True, if max pool size is different.
    * @param config A consumer config object with updated properties.
    */
   public boolean isRestartRequired(GenericConsumerConfig config)
   {
      return (config != null && config.getMaxPoolSize() != m_nMaxPoolSize);
   }

   /**
    * Update this consumer config object with the properties from
    * the specified consumer config object.
    * @param newConfig A consumer config object with updated properties.
    * @throws ResourceException if the operation is not supported.
    */
   public void update(GenericConsumerConfig newConfig) throws ResourceException
   {
   }

   /**
    * @see javax.resource.spi.ActivationSpec#validate()
    */
   public void validate() throws InvalidPropertyException
   {
      if (m_nMaxPoolSize < -1)
      {
         throw new InvalidPropertyException("MaxPoolSize is less than -1");
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(64);

      buf.append(ObjUtil.getShortClassName(this)); 
      buf.append('@');
      buf.append(System.identityHashCode(this));

      return buf.toString();
   }
}
