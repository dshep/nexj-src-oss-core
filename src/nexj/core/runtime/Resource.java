// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

/**
 * Base class for UOW reference counted resources.
 */
public abstract class Resource
{
   // attributes

   /**
    * The resource reference count.
    */
   protected int m_nRef;

   /**
    * The shareability flag.
    */
   protected boolean m_bShareable = isShareableDefault();

   // associations

   /**
    * The resource transaction.
    */
   protected Object m_tx;

   /**
    * The fragment name.
    */
   protected String m_sFragmentName;

   // constructors

   /**
    * Constructs the resource with a reference count of 0.
    */
   protected Resource()
   {
   }
   
   /**
    * Constructs the resource with a given reference count.
    * @param nRef The reference count.
    */
   protected Resource(int nRef)
   {
      assert nRef >= 0;

      m_nRef = nRef;
   }

   // operations

   /**
    * Increments the reference count of the resource.
    */
   public void incRef()
   {
      ++m_nRef;
   }

   /**
    * Decrements the reference count of the resource.
    */
   public void decRef()
   {
      assert m_nRef != 0;

      if (--m_nRef == 0)
      {
         m_bShareable = isShareableDefault();
      }
   }

   /**
    * @return The reference count.
    */
   public int getRef()
   {
      return m_nRef;
   }
   
   /**
    * Sets the resource transaction.
    * @param tx The resource transaction to set.
    */
   public void setTransaction(Object tx)
   {
      m_tx = tx;
   }

   /**
    * @return The resource transaction.
    */
   public Object getTransaction()
   {
      return m_tx;
   }

   /**
    * Sets the fragment name.
    * @param sFragmentName The fragment name to set.
    */
   public void setFragmentName(String sFragmentName)
   {
      m_sFragmentName = sFragmentName;
   }

   /**
    * @return The fragment name.
    */
   public String getFragmentName()
   {
      return m_sFragmentName;
   }

   /**
    * Sets the shareability flag.
    * @param bShareable The shareability flag to set.
    */
   public void setShareable(boolean bShareable)
   {
      m_bShareable = bShareable;
   }

   /**
    * @return The shareability flag.
    */
   public final boolean isShareable()
   {
      return m_bShareable;
   }

   /**
    * @return The default shareability.
    */
   protected boolean isShareableDefault()
   {
      return false;
   }
   
   /**
    * @return The resource manager.
    */
   public abstract ResourceManager getResourceManager();

   /**
    * Releases the resource.
    */
   public abstract void release();

   /**
    * Has this resource already been released.
    */
   public abstract boolean isReleased();

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      String sName = getClass().getName();

      return sName.substring(sName.lastIndexOf('.') + 1) +
         "(tx=" + m_tx + ", rm=" + getResourceManager() + ")";
   }
}
