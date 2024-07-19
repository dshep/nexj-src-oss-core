// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.object;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.integration.CompositeMessagePart;
import nexj.core.runtime.Instance;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Stores information corresponding to object messages.
 */
public class ObjectMessageInfo
{
   // constants

   public final static ObjectMessageInfo EMPTY_INFO = new ObjectMessageInfo()
   {
      protected void addInstanceToDelete(CompositeMessagePart composite, Instance instance)
      {
         throw new IllegalStateException();
      }

      protected Lookup.Iterator getInstancesToDeleteItr()
      {
         return HashTab.EMPTY_ITERATOR;
      }

      public boolean isAudited()
      {
         return true;
      }

      public String getOriginalEvent()
      {
         return null;
      }
   };

   // associations

   /**
    * Map of list of instances to delete, indexed by their corresponding message part metadata.
    */
   protected Lookup m_instanceListByCompositeMap;

   /**
    * The audit flag. True, if the change should be audited.
    */
   protected boolean m_bAudited = true;

   /**
    * The original event name.
    */
   protected String m_sOriginalEvent; 

   // operations

   /**
    * Add instance to delete to list.
    * @param composite The message part metadata corresponding to the instance to delete.
    * @param instance The instance to delete.
    */
   protected void addInstanceToDelete(CompositeMessagePart composite, Instance instance)
   {
      List instanceToDeleteList = null;

      if (m_instanceListByCompositeMap == null)
      {
         m_instanceListByCompositeMap = new HashTab();
      }
      else
      {
         instanceToDeleteList = (List)m_instanceListByCompositeMap.get(composite);
      }

      if (instanceToDeleteList == null)
      {
         instanceToDeleteList = new ArrayList();
         m_instanceListByCompositeMap.put(composite, instanceToDeleteList);
      }

      instanceToDeleteList.add(instance);
   }

   /**
    * @return Lookup.Iterator for the set of instance to delete.
    */
   protected Lookup.Iterator getInstancesToDeleteItr()
   {
      return (m_instanceListByCompositeMap == null) ? HashTab.EMPTY_ITERATOR : m_instanceListByCompositeMap.iterator();
   }

   /**
    * @return The audited flag.
    */
   public boolean isAudited()
   {
      return m_bAudited;
   }

   /**
    * Sets the audited flag.
    * @param bAudited The audited flag.
    */
   public void setAudited(boolean bAudited)
   {
      m_bAudited = bAudited;
   }

   /**
    * @return The original event name.
    */
   public String getOriginalEvent()
   {
      return m_sOriginalEvent;
   }

   /**
    * Sets the original event name.
    * @param sOriginalEvent The original event name.
    */
   public void setOriginalEvent(String sOriginalEvent)
   {
      m_sOriginalEvent = sOriginalEvent;
   }
}
