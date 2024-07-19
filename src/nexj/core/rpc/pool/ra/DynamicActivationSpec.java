package nexj.core.rpc.pool.ra;

import javax.resource.ResourceException;
import javax.resource.spi.ActivationSpec;
import javax.resource.spi.InvalidPropertyException;
import javax.resource.spi.ResourceAdapter;

import nexj.core.util.ObjUtil;

/**
 * Activation spec for the dynamic resource adapter.
 * It is not needed for this adapter, but is required by the JCA spec.
 */
public class DynamicActivationSpec implements ActivationSpec
{
   // attributes

   /**
    * The transactional flag.
    */
   protected boolean m_bTransactional;

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
    * Sets the transactional flag.
    * @param bTransactional The transactional flag to set.
    */
   public void setTransactional(boolean bTransactional)
   {
      m_bTransactional = bTransactional;
   }

   /**
    * @return The transactional flag.
    */
   public boolean isTransactional()
   {
      return m_bTransactional;
   }

   /**
    * @see javax.resource.spi.ActivationSpec#validate()
    */
   public void validate() throws InvalidPropertyException
   {
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this);
   }
}
