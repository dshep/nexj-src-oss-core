// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.ArrayList;

import nexj.core.meta.Attribute;
import nexj.core.meta.persistence.Key;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.Work;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.util.Logger;

/**
 * Work item that executes virtual persistence adapter mappings.
 */
public abstract class VirtualWork extends Work
{
   // constants

   /**
    * Identifies work items that perform a delete operation.
    */
   public final static int DELETE = 1;

   /**
    * Identifies work items that perform an update operation.
    */
   public final static int UPDATE = 2;

   /**
    * Identifies work items that perform a create operation.
    */
   public final static int CREATE = 3;

   // associations

   /**
    * The persistence adapter instance for which this work item was created.
    */
   protected VirtualAdapter m_adapter;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(VirtualWork.class);

   // constructors

   /**
    * Constructs a new work item.
    * 
    * @param instance The instance for which work will be done.
    * @param adapter The adapter.
    */
   public VirtualWork(Instance instance, VirtualAdapter adapter)
   {
      super(instance);

      m_adapter = adapter;
   }

   // operations

   /**
    * Sets the work item data (ONLY FOR WHEN WORK ITEM IS USED AS A LOOKUP KEY).
    * @param instance The instance for which the work item applies.
    */
   protected void setData(Instance instance)
   {
      m_instance = instance;
   }

   /**
    * Executes the work item.
    * @param workArray The array of work items to execute.
    * @param nStart The start index.
    * @param nEnd The end index (exclusive).
    */
   public abstract void execute(Work[] workArray, int nStart, int nEnd);

   /**
    * @see nexj.core.persistence.Work#getAdapter()
    */
   public PersistenceAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * @see nexj.core.persistence.Work#setKeyValue(nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, nexj.core.runtime.Instance)
    */
   public void setKeyValue(Key dstKey, Key srcKey, Instance instance)
   {
      // Keys are not expanded
   }

   /**
    * Puts an attribute/value pair into a transfer object. Takes into account
    * collections (converts from InstanceList to List), OIDs, and null values.
    * @param tobj The transfer object.
    * @param attribute The attribute to set.
    * @param value The value to set.
    */
   public static void setValue(TransferObject tobj, Attribute attribute, Object value)
   {
      if (attribute.getType().isPrimitive())
      {
         tobj.setValue(attribute.getName(), value);
      }
      else
      {
         if (attribute.isCollection() && value != null)
         {
            InstanceList list = (InstanceList)value;
            int nCount = list.size();
            ArrayList collection = new ArrayList(nCount);

            for (int i = 0; i < nCount; i++)
            {
               collection.add(((Instance)list.get(i)).getOID());
            }

            tobj.setValue(attribute.getName(), collection);
         }
         else
         {
            tobj.setValue(attribute.getName(), (value == null) ? null : ((Instance)value).getOID());
         }
      }
   }

   /**
    * @see nexj.core.persistence.Work#compareTo(nexj.core.persistence.Work)
    */
   protected int compareTo(Work work)
   {
      VirtualWork other = (VirtualWork)work;

      return getInstance().getMetaclass().getName().compareTo(other.getInstance().getMetaclass().getName());
   }

   /**
    * @see nexj.core.persistence.Work#equals(nexj.core.persistence.Work)
    */
   public boolean equals(Work work)
   {
      return true;
   }

   /**
    * @see nexj.core.persistence.Work#hashCode()
    */
   public int hashCode()
   {
      return m_instance.hashCode();
   }
}
