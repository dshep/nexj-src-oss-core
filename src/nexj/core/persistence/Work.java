// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Component;
import nexj.core.meta.persistence.DataSource;
import nexj.core.meta.persistence.DataSourceFragment;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.runtime.Instance;

/**
 * Base class for work items in a unit of work.
 * Provides support for topological and total sorting of work items. 
 */
public abstract class Work implements Comparable
{
   // attributes

   /**
    * The data source fragment name.
    */
   protected String m_sFragmentName;

   /**
    * The predecessor count.
    */
   private int m_nPredCount;

   /**
    * The successor count.
    */
   private int m_nSuccCount;
   
   /**
    * True if the work item is empty.
    */
   protected boolean m_bEmpty;

   // associations

   /**
    * The instance to which this work item applies.
    */
   protected Instance m_instance;

   /**
    * The work item persistence mapping.
    */
   protected PersistenceMapping m_mapping;

   /**
    * The successor array: Successor[2*n], Object[2*n+1].
    */
   private Object[] m_succArray;

   // constructors

   /**
    * Constructs the work item.
    * @param instance The instance to which this work item applies.
    */
   protected Work(Instance instance)
   {
      m_instance = instance;
      m_mapping = m_instance.getPersistenceMapping();
      setFragmentName(m_instance.getFragmentName());
   }

   // operations

   /**
    * @return The work item instance.
    */
   public final Instance getInstance()
   {
      return m_instance;
   }

   /**
    * @return The work item persistence mapping.
    */
   public final PersistenceMapping getPersistenceMapping()
   {
      return m_mapping;
   }

   /**
    * @return The work item data source.
    */
   public final DataSource getDataSource()
   {
      return m_mapping.getDataSource();
   }

   /**
    * Sets the data source fragment name.
    * @param sFragmentName The data source fragment name to set.
    */
   public final void setFragmentName(String sFragmentName)
   {
      if (sFragmentName == null)
      {
         sFragmentName = "";
      }

      m_sFragmentName = sFragmentName;
   }

   /**
    * @return The data source fragment name.
    */
   public final String getFragmentName()
   {
      return m_sFragmentName;
   }

   /**
    * @return The data source fragment.
    */
   public final DataSourceFragment getFragment()
   {
      return getDataSource().getFragment(m_sFragmentName);
   }

   /**
    * @return True if the work item is empty.
    */
   public final boolean isEmpty()
   {
      return m_bEmpty;
   }

   /**
    * Increments the predecessor count by 1.
    * @return The new predecessor count.
    */
   public final int incPredCount()
   {
      return ++m_nPredCount;
   }
   
   /**
    * Decrements the predecessor count by 1.
    * @return The new predecessor count.
    */
   public final int decPredCount()
   {
      return --m_nPredCount;
   }
   
   /**
    * @return The predecessor count.
    */
   public final int getPredCount()
   {
      return m_nPredCount;
   }
   
   /**
    * Ignores the work item in the successor list by setting the pred count to -1. 
    */
   public final void ignore()
   {
      m_nPredCount = -1;
      
      for (int i = 0, nCount = getSuccessorCount(); i < nCount; ++i)
      {
         getSuccessor(i).decPredCount();
      }
   }

   /**
    * Adds a successor and associated data to this work item.
    * @param successor The successor to add.
    * @param data The data to add.
    * @param data2 The second data item to add.
    */
   public final void addSuccessor(Work successor, Object data, Object data2)
   {
      int nCount = m_nSuccCount * 3;

      if (nCount == 0)
      {
         m_succArray = new Object[15];
      }
      else if (nCount >= m_succArray.length)
      {
         Object[] succArray = new Object[nCount << 1];

         System.arraycopy(m_succArray, 0, succArray, 0, nCount);
         m_succArray = succArray;
      }
      
      m_succArray[nCount++] = successor;
      m_succArray[nCount++] = data;
      m_succArray[nCount++] = data2;
      ++successor.m_nPredCount;
      ++m_nSuccCount;
   }

   /**
    * Removes the successor with the specified ordinal number.
    * @param nOrdinal The successor ordinal number.
    */
   public final void removeSuccessor(int nOrdinal)
   {
      nOrdinal *= 3;
      
      Work successor = (Work)m_succArray[nOrdinal];
      
      System.arraycopy(m_succArray, nOrdinal + 3, m_succArray, nOrdinal, ((m_nSuccCount-- - 1) * 3) - nOrdinal);
      --successor.m_nPredCount;
   }
   
   /**
    * Removes all the successors.
    */
   public final void removeAllSuccessors()
   {
      int nCount = m_nSuccCount * 3;
      
      for (int i = 0; i < nCount; i += 3)
      {
         --((Work)m_succArray[i]).m_nPredCount;
      }
      
      m_succArray = null;
      m_nSuccCount = 0;
   }
   
   /**
    * Gets the successor by ordinal number.
    * @param nOrdinal The successor ordinal number.
    * @return The successor work item.
    */
   public final Work getSuccessor(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nSuccCount;

      return (Work)m_succArray[nOrdinal * 3];
   }

   /**
    * Gets the successor data by ordinal number.
    * @param nOrdinal The successor ordinal number.
    * @return The successor data.
    */
   public final Object getSuccessorData(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nSuccCount;

      return m_succArray[(nOrdinal * 3) + 1];
   }

   /**
    * Gets the successor second data item by ordinal number.
    * @param nOrdinal The successor ordinal number.
    * @return The successor second data item.
    */
   public final Object getSuccessorData2(int nOrdinal)
   {
      assert nOrdinal >= 0 && nOrdinal < m_nSuccCount;

      return m_succArray[(nOrdinal * 3) + 2];
   }

   /**
    * @return The successor count.
    */
   public final int getSuccessorCount()
   {
      return m_nSuccCount;
   }

   /**
    * Fixes up the successor OIDs. This method should be called on a work item if execution of
    * that item sets (or otherwise changes) the OID of the item's associated instance, e.g.
    * identity/auto-increment columns.
    */
   public void fixup()
   {
      for (int i = 0, nCount = getSuccessorCount(); i < nCount; ++i)
      {
         getSuccessor(i).setKeyValue((Key)getSuccessorData(i), (Key)getSuccessorData2(i), m_instance);
      }
   }

   /**
    * Determines if this work item is batchable with another work item.
    * @param work The other work item.
    * @return True if it is batchable.
    */
   public boolean isBatchableWith(Work work)
   {
      return getDataSource() == work.getDataSource() &&
         m_sFragmentName.equals(work.getFragmentName());
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public final int compareTo(Object o)
   {
      Work work = (Work)o;

      int n;

      if (m_mapping != work.getPersistenceMapping())
      {
         n = getDataSource().getName().compareTo(work.getDataSource().getName());

         if (n != 0)
         {
            return n;
         }
      }

      n = m_sFragmentName.compareTo(work.getFragmentName());

      if (n != 0)
      {
         return n;
      }

      n = (getType() - work.getType());

      if (n != 0)
      {
         return n;
      }

      return compareTo(work);
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public final boolean equals(Object obj)
   {
      if (!(obj instanceof Work))
      {
         return false;
      }

      Work work = (Work)obj;

      if (m_instance != work.m_instance)
      {
         return false;
      }

      return equals(work);
   }

   /**
    * Invokes the persistence hook prepare method.
    */
   public void prepare()
   {
      Component hook = m_mapping.getHook();

      if (hook != null)
      {
         ((PersistenceHook)hook.getInstance(m_instance.getUnitOfWork().getInvocationContext())).prepare(this);
      }
   }

   /**
    * Sets the specified key value.
    * @param dstKey The key to set.
    * @param srcKey The source key, which must be an object key part.
    * @param instance The instance providing the key value.
    */
   public abstract void setKeyValue(Key dstKey, Key srcKey, Instance instance);

   /**
    * @return The persistence adapter.
    */
   public abstract PersistenceAdapter getAdapter();

   /**
    * @return The work item type.
    */
   public abstract int getType();

   /**
    * Compares this work item to another of the same type.
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   protected abstract int compareTo(Work work);

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public abstract boolean equals(Work work);

   /**
    * @see java.lang.Object#hashCode()
    */
   public abstract int hashCode();
}
