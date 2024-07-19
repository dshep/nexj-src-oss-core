// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.IOException;
import java.sql.Timestamp;

import nexj.core.meta.Attribute;
import nexj.core.meta.Metaclass;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.persistence.PersistenceHook;
import nexj.core.persistence.Work;
import nexj.core.rpc.RPCException;
import nexj.core.rpc.RPCUtil;
import nexj.core.rpc.Request;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.runtime.InstanceList;
import nexj.core.runtime.UnitOfWork;
import nexj.core.scripting.Symbol;
import nexj.core.util.Binary;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Holder;
import nexj.core.util.Undefined;

/**
 * Component for serializing an instance event invocation to a binary column.
 */
public class SQLRequestSerializer implements PersistenceHook
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
    * The binary attribute.
    */
   protected Attribute m_binaryAttribute;

   /**
    * The event attribute.
    */
   protected Attribute m_eventAttribute;

   /**
    * The optional start time attribute.
    */
   protected Attribute m_startAttribute;

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
    * Sets the binary attribute name.
    * @param sName The name to set.
    */
   public void setBinaryAttribute(String sName)
   {
      m_binaryAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the event attribute name.
    * @param sName The name to set.
    */
   public void setEventAttribute(String sName)
   {
      m_eventAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * Sets the start time attribute name.
    * @param sName The name to set.
    */
   public void setStartAttribute(String sName)
   {
      m_startAttribute = m_metaclass.getAttribute(sName);
   }

   /**
    * @see nexj.core.persistence.PersistenceHook#addWork(nexj.core.runtime.UnitOfWork, nexj.core.runtime.Instance)
    */
   public void addWork(UnitOfWork uow, Instance instance)
   {
      if (!instance.isDirty(m_objectAttribute.getOrdinal()))
      {
         return;
      }

      Column column = ((RelationalPrimitiveMapping)instance.getPersistenceMapping()
         .getAttributeMapping(m_binaryAttribute)).getColumn();
      SQLAdapter adapter = (SQLAdapter)instance.getAdapter();
      SQLWork work = adapter.findWork(uow, instance, column.getTable());

      if (work == null)
      {
         return;
      }

      Object obj = instance.getValueDirect(m_objectAttribute.getOrdinal());

      if (obj instanceof Instance && ((Instance)obj).getState() != Instance.DELETED &&
         instance.getValueDirect(m_eventAttribute.getOrdinal()) instanceof Symbol)
      {
         work.setValue(column, null);
         addDependency(uow, work, obj, new HashHolder());
      }
   }

   /**
    * Adds a dependency to the work item containing the serialized object.
    * @param uow The unit of work.
    * @param work The work item.
    * @param obj The object, to which to add the dependencies.
    * @param identitySet Set for tracking the object identity.
    */
   public static void addDependency(UnitOfWork uow, SQLWork work, Object obj, Holder identitySet)
   {
      if (obj == null || obj == Undefined.VALUE)
      {
         return;
      }

      if (obj instanceof Instance)
      {
         Instance instance = (Instance)obj;

         if (!identitySet.add(obj))
         {
            return;
         }

         if (instance.getPersistenceMapping() != null)
         {
            if (instance.getOID() == null)
            {
               instance.getAdapter().addDependency(uow, work, instance, null,
                  instance.getPersistenceMapping().getObjectKey(), false);
            }

            return;
         }

         Metaclass metaclass = instance.getMetaclass();

         for (int i = 0, nCount = metaclass.getInstanceAttributeCount(); i < nCount; ++i)
         {
            Attribute attribute = metaclass.getInstanceAttribute(i);

            if (!attribute.getType().isPrimitive())
            {
               addDependency(uow, work, instance.getValueDirect(attribute.getOrdinal()), identitySet);
            }
         }

         return;
      }

      if (obj instanceof InstanceList)
      {
         InstanceList list = (InstanceList)obj;

         for (int i = 0, nCount = list.size(); i < nCount; ++i)
         {
            addDependency(uow, work, list.get(i), identitySet);
         }
      }
   }

   /**
    * @see nexj.core.persistence.PersistenceHook#prepare(nexj.core.persistence.Work)
    */
   public void prepare(Work work)
   {
      Instance instance = work.getInstance();

      if (m_startAttribute != null)
      {
         if (instance.isDirty(m_startAttribute.getOrdinal()))
         {
            instance.getUnitOfWork().setNextTimeout(
               ((Timestamp)instance.getValueDirect(m_startAttribute.getOrdinal())).getTime());
         }
      }

      if (!instance.isDirty(m_objectAttribute.getOrdinal()))
      {
         return;
      }

      Object symbol = instance.getValueDirect(m_eventAttribute.getOrdinal());

      if (!(symbol instanceof Symbol))
      {
         return;
      }

      Column column = ((RelationalPrimitiveMapping)instance.getPersistenceMapping()
         .getAttributeMapping(m_binaryAttribute)).getColumn();
      SQLWork sqlWork = (SQLWork)work;

      if (sqlWork.getTable() == column.getTable())
      {
         TransferObject tobj = (TransferObject)RPCUtil.transferState(
            instance.getValueDirect(m_objectAttribute.getOrdinal()), null, new HashTab(), RPCUtil.TF_ALL);

         Request request = new Request();

         tobj.setEventName(symbol.toString());
         request.addInvocation(tobj);

         try
         {
            sqlWork.setInstanceValue(column, Binary.fromObject(request));
         }
         catch (IOException e)
         {
            throw new RPCException("err.rpc.requestSerialization", e);
         }
      }
   }
}
