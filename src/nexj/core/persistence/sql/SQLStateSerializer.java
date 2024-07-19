// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.persistence.PersistenceHook;
import nexj.core.persistence.Work;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.SerializablePropertyMap;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.HashHolder;
import nexj.core.util.Holder;
import nexj.core.util.PropertyIterator;
import nexj.core.util.Undefined;

/**
 * Component for serializing a flow state.
 */
public class SQLStateSerializer implements PersistenceHook
{
   // associations

   /**
    * The class referenced by this component.
    */
   protected Metaclass m_metaclass;

   /**
    * The object attribute.
    */
   protected Attribute m_objectAttribute;

   /**
    * The class attribute.
    */
   protected Attribute m_classAttribute;

   /**
    * The OID attribute.
    */
   protected Attribute m_oidAttribute;

   /**
    * The variables attribute.
    */
   protected Attribute m_variablesAttribute;

   /**
    * The serialized variables attribute.
    */
   protected Attribute m_serializedVariablesAttribute;

   /**
    * The locking attribute.
    */
   protected Attribute m_lockingAttribute;

   // attributes

   /**
    * The flag for serializing deleted instances.
    */
   protected boolean m_bSerializingDeleted;

   // operations

   /**
    * Sets the class referenced by this component.
    * @param metaclass The class referenced by this component to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      m_metaclass = metaclass;
   }

   /**
    * @return The class referenced by this component.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }

   /**
    * Sets the object attribute name.
    * @param sName The name to set.
    */
   public void setObjectAttribute(String sName)
   {
      m_objectAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the class attribute name.
    * @param sName The name to set.
    */
   public void setClassAttribute(String sName)
   {
      m_classAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the OID attribute name.
    * @param sName The name to set.
    */
   public void setOIDAttribute(String sName)
   {
      m_oidAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the variables attribute name.
    * @param sName The name to set.
    */
   public void setVariablesAttribute(String sName)
   {
      m_variablesAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the serialized variables attribute name.
    * @param sName The name to set.
    */
   public void setSerializedVariablesAttribute(String sName)
   {
      m_serializedVariablesAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the locking attribute name.
    * @param sName The name to set.
    */
   public void setLockingAttribute(String sName)
   {
      m_lockingAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the flag for serializing deleted instances.
    * @param bSerializingDeleted The flag for serializing deleted instances to set.
    */
   public void setSerializingDeleted(boolean bSerializingDeleted)
   {
      m_bSerializingDeleted = bSerializingDeleted;
   }

   /**
    * @return The flag for serializing deleted instances.
    */
   public boolean isSerializingDeleted()
   {
      return m_bSerializingDeleted;
   }

   /**
    * @see nexj.core.persistence.PersistenceHook#addWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance)
    */
   public void addWork(UnitOfWork uow, Instance instance)
   {
      if (m_objectAttribute != null)
      {
         Object obj = instance.getValueDirect(m_objectAttribute.getOrdinal());

         if (obj instanceof Instance)
         {
            Instance inst = (Instance)obj;

            if (inst.getState() != Instance.DELETED || m_bSerializingDeleted)
            {
               if (instance.isDirty(m_objectAttribute.getOrdinal()))
               {
                  if (m_oidAttribute != null)
                  {
                     addDependency(uow, instance, m_oidAttribute, inst);
                  }

                  if (m_classAttribute != null)
                  {
                     addDependency(uow, instance, m_classAttribute, inst);
                  }
               }

               if (m_lockingAttribute != null)
               {
                  addDependency(uow, instance, m_lockingAttribute, inst);
               }
            }
         }
      }

      if (m_variablesAttribute != null)
      {
         SQLAdapter adapter = (SQLAdapter)instance.getAdapter();
         Object obj = instance.getValueDirect(m_variablesAttribute.getOrdinal());

         if (!(obj instanceof Undefined))
         {
            RelationalMapping mapping = (RelationalMapping)instance.getPersistenceMapping();
            RelationalPrimitiveMapping attrMapping = (RelationalPrimitiveMapping)mapping
               .getAttributeMapping(m_serializedVariablesAttribute);

            SQLWork work = (SQLWork)adapter.addPrimitiveWork(uow, instance,
               adapter.findWork(uow, instance, mapping.getPrimaryTable()),
               attrMapping, (obj == null) ? null : "");

            if (work != null)
            {
               work.setValue(attrMapping.getColumn(), null);

               if (obj instanceof SerializablePropertyMap)
               {
                  Holder identitySet = null;

                  for (PropertyIterator itr = ((SerializablePropertyMap)obj).getIterator(); itr.hasNext();)
                  {
                     itr.next();

                     Object value = itr.getValue();

                     if (value instanceof Instance || value instanceof InstanceList)
                     {
                        if (identitySet == null)
                        {
                           identitySet = new HashHolder();
                        }

                        SQLRequestSerializer.addDependency(uow, work, value, identitySet);
                     }
                  }
               }
            }
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceHook#prepare(nexj.core.persistence.Work)
    */
   public void prepare(Work work)
   {
      Instance instance = work.getInstance();
      PersistenceMapping mapping = instance.getPersistenceMapping();
      Object obj;
      Column column;
      SQLWork sqlWork;

      if (m_objectAttribute != null)
      {
         obj = instance.getValueDirect(m_objectAttribute.getOrdinal());

         if (obj instanceof Instance)
         {
            Instance inst = (Instance)obj;

            if (inst.getOID() != null && (inst.getState() != Instance.DELETED || m_bSerializingDeleted))
            {
               if (instance.isDirty(m_objectAttribute.getOrdinal()))
               {
                  if (m_oidAttribute != null)
                  {
                     column = ((RelationalPrimitiveMapping)mapping.getAttributeMapping(m_oidAttribute)).getColumn();
                     sqlWork = (SQLWork)work;

                     if (sqlWork.getTable() == column.getTable())
                     {
                        sqlWork.setInstanceValue(column, inst.getOID().toBinary());
                     }
                  }

                  if (m_classAttribute != null)
                  {
                     column = ((RelationalPrimitiveMapping)mapping.getAttributeMapping(m_classAttribute)).getColumn();
                     sqlWork = (SQLWork)work;

                     if (sqlWork.getTable() == column.getTable())
                     {
                        sqlWork.setInstanceValue(column, inst.getMetaclass().getName());
                     }
                  }
               }

               if (m_lockingAttribute != null)
               {
                  column = ((RelationalPrimitiveMapping)mapping.getAttributeMapping(m_lockingAttribute)).getColumn();
                  sqlWork = (SQLWork)work;

                  if (sqlWork.getTable() == column.getTable())
                  {
                     Attribute lockingAttribute = inst.getPersistenceMapping().getLockingAttribute();

                     sqlWork.setInstanceValue(column, (lockingAttribute != null) ? inst.getValue(lockingAttribute.getOrdinal()) : null);
                  }
               }
            }
         }
      }

      if (m_variablesAttribute != null)
      {
         obj = instance.getValueDirect(m_variablesAttribute.getOrdinal());

         if (obj instanceof SerializablePropertyMap)
         {
            SerializablePropertyMap map = (SerializablePropertyMap)obj;

            column = ((RelationalPrimitiveMapping)mapping.getAttributeMapping(m_serializedVariablesAttribute)).getColumn();
            sqlWork = (SQLWork)work;

            if (sqlWork.getTable() == column.getTable())
            {
               if (column.getType().equals(Primitive.STRING))
               {
                  sqlWork.setInstanceValue(column, map.serializeValues(instance.getUnitOfWork().getInvocationContext()));
               }
               else
               {
                  sqlWork.setInstanceValue(column, map.serializeValuesToBinary(instance.getUnitOfWork().getInvocationContext()));
               }
            }
         }
      }
   }

   /**
    * Adds a dependency between an instance and a work item corresponding to an attribute.
    * @param uow The unit of work.
    * @param instance The instance providing the attribute value.
    * @param attribute The attribute, which will store a value from the dependent instance.
    * @param obj The dependent instance.
    */
   protected static void addDependency(UnitOfWork uow, Instance instance, Attribute attribute, Instance obj)
   {
      SQLAdapter adapter = (SQLAdapter)instance.getAdapter();
      Column column = ((RelationalPrimitiveMapping)instance.getPersistenceMapping().getAttributeMapping(attribute)).getColumn();
      SQLWork work = adapter.findWork(uow, instance, column.getTable());

      if (work != null)
      {
         work.setValue(column, null);

         if (obj.getPersistenceMapping() != null && obj.getOID() == null)
         {
            obj.getAdapter().addDependency(uow, work, obj, null,
               obj.getPersistenceMapping().getObjectKey(), false);
         }
      }
   }
}
