// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.ArrayList;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.virtual.VirtualDataSourceFragment;
import nexj.core.meta.persistence.virtual.VirtualMapping;
import nexj.core.meta.persistence.virtual.VirtualPrimitiveMapping;
import nexj.core.meta.persistence.virtual.WorkMapping;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Work;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;

/**
 * Work item that executes virtual persistence adapter create mappings.
 */
public class VirtualCreate extends VirtualWork
{
   // constants

   /**
    * The name for the transfer object event.
    */
   public final static String EVENT = "create";

   // constructors

   /**
    * Creates a new creation work item.
    * @param instance The instance to create.
    * @param adapter The adapter.
    */
   public VirtualCreate(Instance instance, VirtualAdapter adapter)
   {
      super(instance, adapter);
   }

   // operations

   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return CREATE;
   }

   /**
    * @see nexj.core.persistence.virtual.VirtualWork#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, final int nStart, final int nEnd)
   {
      Metaclass metaclass = m_instance.getMetaclass();
      int nAttributeCount = metaclass.getInstanceAttributeCount();
      VirtualMapping mapping = (VirtualMapping)m_mapping;
      WorkMapping operation = mapping.getCreateMapping();
      Attribute lockingAttribute = mapping.getLockingAttribute();
      int nLockingOrdinal = (lockingAttribute == null) ? -1 : lockingAttribute.getOrdinal();
      String sLockingAttributeName = (lockingAttribute == null) ? null :  lockingAttribute.getName();
      VirtualDataSourceFragment fragment = (VirtualDataSourceFragment)getFragment();
      ArrayList tobjList = new ArrayList(nEnd - nStart);

      for (int i = nStart; i < nEnd; i++)
      {
         Instance instance = workArray[i].getInstance();
         TransferObject tobj = new TransferObject(instance.getOID(), metaclass.getName(), EVENT, nAttributeCount);

         for (int k = 0; k < nAttributeCount; k++)
         {
            if (instance.isDirty(k))
            {
               Attribute attribute = metaclass.getInstanceAttribute(k);
               AttributeMapping attrMapping = m_mapping.getAttributeMapping(attribute);

               if (attrMapping != null)
               {
                  int nKeyPart = (attrMapping instanceof VirtualPrimitiveMapping) ?
                     ((VirtualPrimitiveMapping)attrMapping).getObjectKeyPart() : -1;
                  Object value;

                  if (nKeyPart >= 0 && instance.getOID() != null)
                  {
                     value = instance.getOID().getValue(nKeyPart);
                  }
                  else
                  {
                     value = instance.getValue(k);
                  }

                  setValue(tobj, attribute, value);
               }
            }
         }

         tobjList.add(tobj);
      }

      try
      {
         operation.invoke(tobjList, m_adapter.getInvocationContext().getMachine(), fragment);
      }
      finally
      {
         // Sets the locking value and OID, if provided by the create mapping.
         for (int k = nStart; k < nEnd; k++)
         {
            VirtualCreate work = (VirtualCreate)workArray[k];
            Instance instance = work.getInstance();
            TransferObject tobj = (TransferObject)tobjList.get(k - nStart);

            if (nLockingOrdinal >= 0)
            {
               instance.setValueDirect(nLockingOrdinal,
                  tobj.findValue(sLockingAttributeName, instance.getValueDirect(nLockingOrdinal)));
            }

            if (tobj.getOID() != null)
            {
               instance.setOID(tobj.getOID());
            }

            if (instance.getOID() == null)
            {
               throw new PersistenceException("err.persistence.requiredOID", new Object[]{instance.getMetaclass().getName()});
            }

            work.fixup();
         }
      }
   }
}
