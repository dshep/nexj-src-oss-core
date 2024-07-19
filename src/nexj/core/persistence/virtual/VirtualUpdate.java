// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.virtual.UpdateMapping;
import nexj.core.meta.persistence.virtual.UpdateMappingCase;
import nexj.core.meta.persistence.virtual.VirtualClassMapping;
import nexj.core.meta.persistence.virtual.VirtualDataSourceFragment;
import nexj.core.meta.persistence.virtual.VirtualMapping;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Work;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.util.Logger;
import nexj.core.util.ObjUtil;

/**
 * Work item that executes virtual persistence adapter update mappings.
 */
public class VirtualUpdate extends VirtualWork
{
   // constants

   /**
    * The name for the transfer object event.
    */
   public final static String EVENT = "update";

   // associations

   /**
    * The bitmap of attributes being updated.
    */
   protected BitSet m_attributeSet;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(VirtualUpdate.class);

   // constructors

   /**
    * Constructs a new update work item.
    * @param instance The instance to update.
    * @param adapter The adapter.
    */
   public VirtualUpdate(Instance instance, VirtualAdapter adapter)
   {
      super(instance, adapter);

      Metaclass metaclass = instance.getMetaclass();
      int nCount = metaclass.getInstanceAttributeCount();

      m_attributeSet = new BitSet(nCount);

      for (int i = 0; i < nCount; i++)
      {
         if (instance.isDirty(i))
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);
            AttributeMapping attributeMapping = m_mapping.getAttributeMapping(attribute);

            if (attributeMapping != null && (attribute.getType().isPrimitive() ||
               !((VirtualClassMapping)attributeMapping).getKey(false).isObjectKey()))  // Skips the updates for non-inner attributes.
            {
               m_attributeSet.set(i);
            }
         }
      }

      m_bEmpty = m_attributeSet.isEmpty();
   }

   // operations

   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return UPDATE;
   }

   /**
    * Orders the updates so that work units that update the same attributes are
    * grouped together.
    * @see nexj.core.persistence.virtual.VirtualWork#compareTo(nexj.core.persistence.Work)
    */
   protected int compareTo(Work work)
   {
      VirtualUpdate other = (VirtualUpdate)work;
      int nDifference;

      nDifference = other.getInstance().getMetaclass().getName().compareTo(getInstance().getMetaclass().getName());

      if (nDifference != 0)
      {
         return nDifference;
      }

      BitSet attrSet = m_attributeSet;
      BitSet otherAttrSet = other.m_attributeSet;

      nDifference = attrSet.cardinality() - otherAttrSet.cardinality();

      if (nDifference != 0)
      {
         return nDifference;
      }

      BitSet diffSet = (BitSet)attrSet.clone();

      diffSet.xor(otherAttrSet);

      int i = diffSet.nextSetBit(0);

      if (i < 0)
      {
         return 0;
      }

      return otherAttrSet.get(i) ? -1 : 1;
   }

   /**
    * @see nexj.core.persistence.virtual.VirtualWork#execute(nexj.core.persistence.Work[], int, int)
    */
   public void execute(Work[] workArray, int nStart, int nEnd)
   {
      // All items have the same attributes to update
      VirtualMapping mapping = (VirtualMapping)m_mapping;
      UpdateMapping updateMapping = mapping.getUpdateMapping();
      Metaclass metaclass = mapping.getMetaclass();
      List updateList = updateMapping.getUpdateCases(m_attributeSet);

      // Execute updates
      Attribute lockingAttribute = mapping.getLockingAttribute();
      int nLockingOrdinal = (lockingAttribute == null) ? -1 : lockingAttribute.getOrdinal();
      String sLockingAttributeName = (lockingAttribute == null) ? null : lockingAttribute.getName();
      VirtualDataSourceFragment fragment = (VirtualDataSourceFragment)getFragment();

      for (int i = 0, nSize = updateList.size(); i < nSize; i++)
      {
         UpdateMappingCase update = (UpdateMappingCase)updateList.get(i);
         BitSet updateAttrSet = update.getAttributeSet();
         ArrayList tobjList =  new ArrayList(nEnd - nStart);
         int nUpdateAttrCount = updateAttrSet.cardinality() + ((nLockingOrdinal >= 0) ? 1 : 0);
         boolean bIgnoreDirty = !update.isDirty();
         boolean bFull = update.isFull();

         for (int k = nStart; k < nEnd; k++)
         {
            VirtualUpdate work = (VirtualUpdate)workArray[k];
            Instance instance = work.getInstance();
            TransferObject tobj = new TransferObject(instance.getOID(), metaclass.getName(), EVENT, nUpdateAttrCount);

            if (bFull)
            {
               for (int m = 0, nCount = metaclass.getInstanceAttributeCount(); m < nCount; m++)
               {
                  Attribute attribute = metaclass.getInstanceAttribute(m);

                  if (bIgnoreDirty || instance.isDirty(m))
                  {
                     setValue(tobj, attribute, instance.getValue(m));
                  }
               }
            }
            else
            {
               for (int m = updateAttrSet.nextSetBit(0); m >= 0; m = updateAttrSet.nextSetBit(m + 1))
               {
                  Attribute attribute = metaclass.getInstanceAttribute(m);

                  if (bIgnoreDirty || instance.isDirty(m))
                  {
                     setValue(tobj, attribute, instance.getValue(m));
                  }
               }
            }

            if (nLockingOrdinal >= 0)
            {
               tobj.setValue(sLockingAttributeName, instance.getValue(nLockingOrdinal));
            }

            tobjList.add(tobj);
         }

         boolean bError = true;

         try
         {
            update.invoke(tobjList, m_adapter.getInvocationContext().getMachine(), fragment);
            bError = false;
         }
         finally
         {
            // Set new locking values
            if (nLockingOrdinal >= 0)
            {
               for (int k = nStart; k < nEnd; k++)
               {
                  Instance instance = ((VirtualUpdate)workArray[k]).getInstance();
                  TransferObject tobj = (TransferObject)tobjList.get(k - nStart);
                  Object locking = tobj.findValue(sLockingAttributeName);

                  if (locking == null || ObjUtil.equal(locking, instance.getValue(nLockingOrdinal)))
                  {
                     if (!bError)
                     {
                        PersistenceException ex = new PersistenceException("err.persistence.virtual.lockingNotModified",
                           new Object[]{sLockingAttributeName, instance.getMetaclass().getName()});

                        ex.setValue("result", tobj);

                        throw ex;
                     }
                  }
                  else
                  {
                     instance.setValueDirect(nLockingOrdinal, locking);
                  }
               }
            }
         }
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder("Virtual update of class ");

      buf.append(m_instance.getMetaclass().getName());

      if (m_attributeSet != null && !m_attributeSet.isEmpty())
      {
         buf.append(" attributes (");
         UpdateMapping.appendAttrSet(buf, m_attributeSet, m_instance.getMetaclass());
         buf.append(')');
      }
      else
      {
         buf.append(", no attributes updated");
      }

      return buf.toString();
   }
}
