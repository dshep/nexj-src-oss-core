package nexj.core.util.pool.resource;

import javax.transaction.xa.XAResource;

import nexj.core.util.Logger;

/**
 * Generic pooled resource implementation.
 */
public abstract class GenericResource implements Resource
{
   // attributes

   /**
    * The resource reference count.
    */
   protected int m_nRefCount;

   /**
    * The time of last use in milliseconds since 1-Jan-1970 UTC.
    */
   protected long m_lTime;

   // associations

   /**
    * The resource pool. Can be null if not associated.
    */
   protected ResourcePool m_pool;

   /**
    * The class logger.
    */
   private final static Logger s_logger = Logger.getLogger(GenericResource.class);

   // operations

   /**
    * @see nexj.core.util.pool.resource.Resource#getXAResource()
    */
   public XAResource getXAResource()
   {
      return null;
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#setPool(nexj.core.util.pool.resource.ResourcePool)
    */
   public void setPool(ResourcePool pool)
   {
      m_pool = pool;
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#setTime(long)
    */
   public void setTime(long lTime)
   {
      m_lTime = lTime;
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#getTime()
    */
   public long getTime()
   {
      return m_lTime;
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#reference()
    */
   public synchronized void reference()
   {
      ++m_nRefCount;
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#release()
    */
   public void release()
   {
      ResourcePool pool;

      synchronized (this)
      {
         if (--m_nRefCount != 0)
         {
            return;
         }

         pool = m_pool;
      }

      if (pool != null)
      {
         pool.release(this);
      }
      else
      {
         dispose();
      }
   }

   /**
    * @see nexj.core.util.pool.resource.Resource#dispose()
    */
   public final void dispose()
   {
      if (m_pool != null)
      {
         m_pool.dispose(this);
      }
      else
      {
         try
         {
            drop();
         }
         catch (Throwable t)
         {
            if (getLogger().isDebugEnabled())
            {
               getLogger().debug("Unable to dispose of " + this, t);
            }
         }
      }
   }

   /**
    * @return The logger.
    */
   protected Logger getLogger()
   {
      return s_logger;
   }

   /**
    * Releases any physical resources.
    */
   protected abstract void drop() throws Throwable;
}
