// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.ObjUtil;

/**
 * Pointcut implementation helper.
 */
public abstract class PointcutHelper implements Cloneable
{
   // attributes

   /**
    * Aspect count.
    */
   protected int m_nAspectCount;
   
   /**
    * Aspect override count.
    */
   protected int m_nAspectOverrideCount;
   
   // associations

   /**
    * Aspect array.
    */
   protected Aspect[] m_aspectArray = new Aspect[1];

   /**
    * Aspect override array: Aspect[2*n], bInclusive[2*n+1].
    */
   protected Object[] m_aspectOverrideArray = new Object[2];

   // operations

   /**
    * @return The container.
    */
   protected abstract Pointcut getContainer();

   /**
    * @return The container type name.
    */
   protected abstract String getContainerType();

   /**
    * @see nexj.core.meta.Pointcut#addAspect(nexj.core.meta.Aspect)
    */
   public void addAspect(Aspect aspect) throws MetadataException
   {
      for (int i = 0; i < m_nAspectCount; ++i)
      {
         if (m_aspectArray[i] == aspect)
         {
            throw new MetadataException("err.meta." + getContainerType() + "AspectDup",
               new Object[]{aspect.getName(), getContainer().getName()}); 
         }
      }

      if (m_nAspectCount == m_aspectArray.length)
      {
         Aspect[] array = new Aspect[m_nAspectCount << 1];
         
         System.arraycopy(m_aspectArray, 0, array, 0, m_nAspectCount);
         m_aspectArray = array;
      }

      m_aspectArray[m_nAspectCount++] = aspect;
   }

   /**
    * @see nexj.core.meta.Pointcut#removeAspect(nexj.core.meta.Aspect)
    */
   public boolean removeAspect(Aspect aspect)
   {
      for (int i = 0; i < m_nAspectCount; ++i)
      {
         if (m_aspectArray[i] == aspect)
         {
            System.arraycopy(m_aspectArray, i + 1, m_aspectArray, i, m_nAspectCount - i - 1);
            m_aspectArray[--m_nAspectCount] = null;

            return true;
         }
      }

      return false;
   }
   
   /**
    * @see nexj.core.meta.Pointcut#hasAspect(nexj.core.meta.Aspect)
    */
   public boolean hasAspect(Aspect aspect)
   {
      for (int i = 0; i < m_nAspectCount; ++i)
      {
         if (m_aspectArray[i] == aspect)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspect(int)
    */
   public Aspect getAspect(int nOrdinal)
   {
      if (nOrdinal >= m_nAspectCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return m_aspectArray[nOrdinal];
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectCount()
    */
   public int getAspectCount()
   {
      return m_nAspectCount;
   }

   /**
    * @see nexj.core.meta.Pointcut#addAspectOverride(nexj.core.meta.Aspect, boolean)
    */
   public void addAspectOverride(Aspect aspect, boolean bInclusive) throws MetadataException
   {
      if (findAspectOverride(aspect) >= 0)
      {
         throw new MetadataException("err.meta." + getContainerType() + "AspectDup",
            new Object[]{aspect.getName(), getContainer().getName()}); 
      }

      int i = m_nAspectOverrideCount << 1;

      if (i == m_aspectOverrideArray.length)
      {
         Object[] array = new Object[i << 1];
         
         System.arraycopy(m_aspectOverrideArray, 0, array, 0, i);
         m_aspectOverrideArray = array;
      }

      m_aspectOverrideArray[i] = aspect;
      m_aspectOverrideArray[i + 1] = Boolean.valueOf(bInclusive);
      ++m_nAspectOverrideCount;

      if (bInclusive && !hasAspect(aspect))
      {
         addAspect(aspect);
      }
   }
   
   /**
    * @see nexj.core.meta.Pointcut#removeAspectOverride(nexj.core.meta.Aspect)
    */
   public boolean removeAspectOverride(Aspect aspect)
   {
      for (int i = 0, n = m_nAspectOverrideCount << 1; i < n; i += 2)
      {
         if (m_aspectOverrideArray[i] == aspect)
         {
            System.arraycopy(m_aspectOverrideArray, i + 2, m_aspectOverrideArray, i, n - i - 2);
            m_aspectOverrideArray[n - 1] = m_aspectOverrideArray[n - 2] = null;
            --m_nAspectOverrideCount;
            removeAspect(aspect);
            
            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.meta.Pointcut#findAspectOverride(nexj.core.meta.Aspect)
    */
   public int findAspectOverride(Aspect aspect)
   {
      for (int i = 0; i < m_nAspectOverrideCount; ++i)
      {
         if (m_aspectOverrideArray[i << 1] == aspect)
         {
            return i;
         }
      }

      return -1;
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverride(int)
    */
   public Aspect getAspectOverride(int nOrdinal)
   {
      if (nOrdinal >= m_nAspectOverrideCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return (Aspect)m_aspectOverrideArray[nOrdinal << 1];
   }

   /**
    * @see nexj.core.meta.Pointcut#isAspectOverrideInclusive(int)
    */
   public boolean isAspectOverrideInclusive(int nOrdinal)
   {
      if (nOrdinal >= m_nAspectOverrideCount)
      {
         throw new ArrayIndexOutOfBoundsException(nOrdinal);
      }

      return ((Boolean)m_aspectOverrideArray[(nOrdinal << 1) + 1]).booleanValue();
   }

   /**
    * @see nexj.core.meta.Pointcut#getAspectOverrideCount()
    */
   public int getAspectOverrideCount()
   {
      return m_nAspectOverrideCount;
   }

   /**
    * Clones the pointcut helper.
    * @param lookup The aspect lookup.
    * @return The new pointcut helper.
    */
   public PointcutHelper clone(AspectLookup lookup)
   {
      try
      {
         PointcutHelper helper = (PointcutHelper)super.clone();

         helper.m_aspectArray = new Aspect[m_aspectArray.length];

         for (int i = 0; i < m_nAspectCount; ++i)
         {
            helper.m_aspectArray[i] = lookup.get(m_aspectArray[i].getName());
         }

         helper.m_aspectOverrideArray = new Object[m_aspectOverrideArray.length];

         for (int i = 0; i < m_nAspectOverrideCount; ++i)
         {
            helper.m_aspectOverrideArray[i << 1] = lookup.get(((Aspect)m_aspectOverrideArray[i << 1]).getName());
            helper.m_aspectOverrideArray[(i << 1) + 1] = m_aspectOverrideArray[(i << 1) + 1];
         }

         return helper;
      }
      catch (CloneNotSupportedException e)
      {
         ObjUtil.rethrow(e);

         return null;
      }
   }

   // inner classes

   /**
    * Interface for looking up aspects.
    */
   public interface AspectLookup
   {
      /**
       * Gets an aspect.
       * @param sName The aspect name.
       * @return The aspect.
       * @throws MetadataException if the aspect is not found.
       */
      Aspect get(String sName) throws MetadataException;
   }
}
