// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import java.util.List;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.meta.persistence.ClassMapping;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.PersistenceMapping;
import nexj.core.persistence.operator.ConversionMapper;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.InvocationContext;
import nexj.core.runtime.InvocationContextAware;
import nexj.core.runtime.InvocationContextHolder;
import nexj.core.runtime.ResourceManager;
import nexj.core.runtime.UnitOfWork;
import nexj.core.util.ObjUtil;
import nexj.core.util.Undefined;

/**
 * Generic persistence adapter implementation.
 */
public abstract class GenericPersistenceAdapter implements PersistenceAdapter, InvocationContextAware, InvocationContextHolder, ResourceManager
{
   // attributes

   /**
    * The fragmented data source flag.
    */
   protected boolean m_bFragmented;

   // associations

   /**
    * The invocation context.
    */
   protected InvocationContext m_context;

   // operations

   /**
    * Sets the fragmented data source flag.
    * @param bFragmented The fragmented data source flag to set.
    */
   public void setFragmented(boolean bFragmented)
   {
      m_bFragmented = bFragmented;
   }

   /**
    * @return The fragmented data source flag.
    */
   public boolean isFragmented()
   {
      return m_bFragmented;
   }

   /**
    * @see nexj.core.runtime.InvocationContextAware#setInvocationContext(nexj.core.runtime.InvocationContext)
    */
   public void setInvocationContext(InvocationContext context)
   {
      m_context = context;
   }

   /**
    * @see nexj.core.runtime.InvocationContextHolder#getInvocationContext()
    */
   public InvocationContext getInvocationContext()
   {
      return m_context;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#read(nexj.core.persistence.Query)
    */
   public InstanceList read(Query query) throws PersistenceException
   {
      Cursor cursor = openCursor(query);

      try
      {
         return cursor.next(-1);
      }
      finally
      {
         cursor.close();
      }
   }

   /**
    * It is always safe to return false; however, doing so will impact the ability of the
    * framework to perform certain optimizations.
    * @see nexj.core.persistence.PersistenceAdapter#isUnique(nexj.core.persistence.Query, java.util.List)
    */
   public boolean isUnique(Query query, List sourceList)
   {
      return false;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#isSupported(nexj.core.persistence.Operator, int)
    */
   public boolean isSupported(Operator op, int nFlags)
   {
      return (nFlags & Operator.NORMALIZE_EXPRESSION) == Operator.NORMALIZE_WHERE;
   }

   /**
    * @see nexj.core.persistence.PersistenceAdapter#getConversionMapper()
    */
   public ConversionMapper getConversionMapper() throws PersistenceException
   {
      return Operator.getConversionMapper();
   }

   /**
    * This is the correct implementation for adapters where denormalization is not supported. 
    * @see nexj.core.persistence.PersistenceAdapter#getQuery(nexj.core.persistence.Source)
    */
   public Query getQuery(Source source)
   {
      return source.getQuery();
   }

   /**
    * Adds an extension work item for a primitive attribute.
    * @param uow The unit of work.
    * @param instance The instance.
    * @param primaryWork The primary work item.
    * @param attributeMapping The primitive attribute mapping.
    * @param value The value to set.
    * @return The work item for the primitive attribute.
    */
   protected Work addPrimitiveWork(UnitOfWork uow, Instance instance,
      Work primaryWork, AttributeMapping attributeMapping, Object value)
   {
      return primaryWork;
   }

   /**
    * Adds an extension work item for an association.
    * @param uow The unit of work.
    * @param instance The instance.
    * @param primaryWork The primary work item.
    * @param assocMapping The association mapping.
    * @return The work item for the association.
    */
   protected Work addClassWork(UnitOfWork uow, Instance instance,
      Work primaryWork, ClassMapping assocMapping)
   {
      return primaryWork;
   }

   /**
    * Determines whether to add successors or predecessors to the create work item.
    * @param work The create work item.
    * @param assocMapping The mapping on the association attribute.
    * @param bOID True if the instance being created has an object key; false otherwise.
    * @return True to add successors to the work item; false to add predecessors.
    */
   protected boolean isCreateSuccessor(Work work, ClassMapping assocMapping, boolean bOID)
   {
      return assocMapping.getKey(false).isObjectKeyPart();
   }

   /**
    * Adds create dependencies for the work item.
    * @param uow The unit of work.
    * @param instance The instance being created.
    * @param primaryWork The work item for which to add the dependencies.
    */
   protected void addCreateDependencies(UnitOfWork uow, Instance instance, Work primaryWork)
   {
      boolean bOID = (instance.getOID() != null);
      Metaclass metaclass = instance.getMetaclass();
      PersistenceMapping mapping = instance.getPersistenceMapping();

      for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
      {
         Attribute attribute = metaclass.getInstanceAttribute(i);
         AttributeMapping attributeMapping = mapping.getAttributeMapping(attribute);

         if (attributeMapping == null)
         {
            continue;
         }

         Object value = instance.getValueDirect(i);

         // The value cannot be undefined, the Instance.validate() has taken care of that.

         if (attribute.getType().isPrimitive())
         {
            addPrimitiveWork(uow, instance, primaryWork, attributeMapping, value);
         }
         else
         {
            InstanceList list = null;
            Instance inst = null;
            boolean bNull;

            if (attribute.isCollection())
            {
               list = (InstanceList)value;
               bNull = (list == null || list.isEmpty());
            }
            else
            {
               inst = (Instance)value;
               bNull = (inst == null);
            }

            if (!bNull)
            {
               // TODO: Insert with composition mappings.
               ClassMapping assocMapping = (ClassMapping)attributeMapping;
               boolean bSuccessor = isCreateSuccessor(primaryWork, assocMapping, bOID);

               if (bSuccessor)
               {
                  if (list == null)
                  {
                     inst.getAdapter().addDependency(uow, primaryWork, inst,
                        assocMapping.getKey(true), assocMapping.getKey(false), true);
                  }
                  else
                  {
                     for (int k = 0; k < list.getCount(); ++k)
                     {
                        inst = list.getInstance(k);

                        if (inst != null)
                        {
                           inst.getAdapter().addDependency(uow, primaryWork, inst,
                              assocMapping.getKey(true), assocMapping.getKey(false), true);
                        }
                     }
                  }
               }
               else
               {
                  Work work = addClassWork(uow, instance, primaryWork, assocMapping);

                  if (list != null)
                  {
                     inst = list.getInstance(0);
                  }

                  if (inst.getOID() != null)
                  {
                     work.setKeyValue(assocMapping.getKey(false), assocMapping.getKey(true), inst);

                     if (assocMapping.getKey(false) == mapping.getObjectKey())
                     {
                        instance.setOID(inst.getOID());
                     }
                  }
                  else
                  {
                     inst.getAdapter().addDependency(uow, work, inst,
                        assocMapping.getKey(false), assocMapping.getKey(true), false);
                  }
               }
            }
         }

         if (attributeMapping.isDenormalized())
         {
            uow.addDenorm(instance, attributeMapping);
         }
      }

      if (bOID)
      {
         for (int i = primaryWork.getSuccessorCount() - 1; i >= 0; --i)
         {
            primaryWork.getSuccessor(i).setKeyValue((Key)primaryWork.getSuccessorData(i), (Key)primaryWork.getSuccessorData2(i), instance);
         }

         primaryWork.removeAllSuccessors();
      }
   }

   /**
    * Adds update dependencies for the work item.
    * @param uow The unit of work.
    * @param instance The instance being updated.
    * @param primaryWork The work item for which to add the dependencies.
    */
   protected void addUpdateDependencies(UnitOfWork uow, Instance instance, Work primaryWork)
   {
      Metaclass metaclass = instance.getMetaclass();
      PersistenceMapping mapping = instance.getPersistenceMapping();

      for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
      {
         Attribute attribute = metaclass.getInstanceAttribute(i);
         AttributeMapping attributeMapping = mapping.getAttributeMapping(attribute);

         if (attributeMapping == null)
         {
            continue;
         }

         Object value = instance.getValueDirect(i);

         if (value == Undefined.VALUE || ObjUtil.equal(value, instance.getOldValueDirect(i)))
         {
            continue;
         }

         if (attribute.getType().isPrimitive())
         {
            addPrimitiveWork(uow, instance, primaryWork, attributeMapping, value);
         }
         else
         {
            InstanceList list = null;
            Instance inst = null;
            boolean bNull;

            if (attribute.isCollection())
            {
               list = (InstanceList)value;
               bNull = (list == null || list.isEmpty());
            }
            else
            {
               inst = (Instance)value;
               bNull = (inst == null);
            }

            ClassMapping assocMapping = (ClassMapping)attributeMapping;
            boolean bSuccessor;

            // TODO: Update composition mappings

            if (assocMapping.getKey(false).isObjectKey())
            {
               bSuccessor = true;
            }
            else if (assocMapping.getKey(true).isObjectKey())
            {
               bSuccessor = false;
            }
            else if (attribute.isCollection())
            {
               bSuccessor = true;
            }
            else
            {
               bSuccessor = false;
            }

            if (bSuccessor)
            {
               if (!bNull)
               {
                  if (list == null)
                  {
                     inst.getAdapter().addDependency(uow, primaryWork, inst,
                        assocMapping.getKey(true), assocMapping.getKey(false), true);
                  }
                  else
                  {
                     for (int k = 0; k < list.getCount(); ++k)
                     {
                        inst = list.getInstance(k);

                        if (inst != null)
                        {
                           inst.getAdapter().addDependency(uow, primaryWork, inst,
                              assocMapping.getKey(true), assocMapping.getKey(false), true);
                        }
                     }
                  }
               }
            }
            else
            {
               Work work = addClassWork(uow, instance, primaryWork, assocMapping);

               if (bNull)
               {
                  if (assocMapping.getKey(false) != mapping.getObjectKey())
                  {
                     work.setKeyValue(assocMapping.getKey(false), assocMapping.getKey(true), null);
                  }
               }
               else
               {
                  if (list != null)
                  {
                     inst = list.getInstance(0);
                  }

                  if (inst.getOID() != null)
                  {
                     if (assocMapping.getKey(false) != mapping.getObjectKey())
                     {
                        work.setKeyValue(assocMapping.getKey(false), assocMapping.getKey(true), inst);
                     }
                  }
                  else
                  {
                     inst.getAdapter().addDependency(uow, work, inst,
                        assocMapping.getKey(false), assocMapping.getKey(true), false);
                  }
               }
            }
         }

         if (attributeMapping.isDenormalized())
         {
            uow.addDenorm(instance, attributeMapping);
         }
      }
   }
}
