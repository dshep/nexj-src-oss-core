// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.ArrayList;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.virtual.WorkMapping;
import nexj.core.meta.persistence.virtual.VirtualDataSourceFragment;
import nexj.core.meta.persistence.virtual.VirtualMapping;
import nexj.core.persistence.Work;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;

/**
 * Work item that executes virtual persistence adapter delete mappings.
 */
public class VirtualDelete extends VirtualWork
{
   // constants

   /**
    * The name for the transfer object event.
    */
   public final static String EVENT = "delete";

   // constructors

   /**
    * Creates a new deletion work item.
    * @param instance The instance to delete.
    * @param adapter The adapter.
    */
   public VirtualDelete(Instance instance, VirtualAdapter adapter)
   {
      super(instance, adapter);
   }

   // operations

   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return DELETE;
   }

   /**
    * @see nexj.core.persistence.virtual.VirtualWork#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, int nStart, int nEnd)
   {
      Metaclass metaclass = m_instance.getMetaclass();
      VirtualMapping mapping = (VirtualMapping)m_mapping;
      WorkMapping operation = mapping.getDeleteMapping();
      Attribute lockingAttribute = mapping.getLockingAttribute();
      int nLockingOrdinal = (lockingAttribute == null) ? -1 : lockingAttribute.getOrdinal();
      String sLockingAttributeName = (lockingAttribute == null) ? null : lockingAttribute.getName();
      VirtualDataSourceFragment fragment = (VirtualDataSourceFragment)getFragment();
      ArrayList tobjList = new ArrayList(nEnd - nStart);

      for (int i = nStart; i < nEnd; i++)
      {
         Instance instance = workArray[i].getInstance();
         TransferObject tobj;

         if (nLockingOrdinal >= 0)
         {
            tobj = new TransferObject(instance.getOID(), metaclass.getName(), EVENT, 1);
            tobj.setValue(sLockingAttributeName, instance.getValue(nLockingOrdinal));
         }
         else
         {
            tobj = new TransferObject(instance.getOID(), metaclass.getName(), EVENT, 0);
         }

         tobjList.add(tobj);
      }

      operation.invoke(tobjList, m_adapter.getInvocationContext().getMachine(), fragment);
   }
}
