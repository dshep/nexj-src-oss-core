// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import java.util.Iterator;
import java.util.List;

import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.meta.integration.format.object.ObjectMessagePartMapping;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.Instance;
import nexj.core.util.Lookup;

/**
 * Object message formatter that is not associated with a synchronization target.
 */
public class BasicFormatter extends ObjectMessageFormatterStrategy
{
   /**
    * @see nexj.core.integration.format.object.ObjectMessageFormatterStrategy#updateInstance(boolean, TransferObject, Instance, CompositeMessagePart, ObjectMessagePartMapping)
    */
   protected Object updateInstance(boolean bNew, TransferObject tobj, Instance instance,
      CompositeMessagePart composite, ObjectMessagePartMapping mapping)
   {
      updateInstance(composite, tobj, instance, bNew, null);

      ObjectMessageInfo infoObject = (ObjectMessageInfo)getInfoObject(tobj);

      for (Lookup.Iterator compositeItr = infoObject.getInstancesToDeleteItr(); compositeItr.hasNext();)
      {
         compositeItr.next();

         for (Iterator instanceItr = ((List)compositeItr.getValue()).iterator(); instanceItr.hasNext();)
         {
            TransferObject tobjToDelete = new TransferObject();

            setEventName(tobjToDelete, "delete");
            m_invocationList.add(tobjToDelete);
            m_instanceByTobjMap.put(tobjToDelete, instanceItr.next());
         }
      }

      return null;
   }
}