// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.virtual;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import nexj.core.persistence.OID;
import nexj.core.rpc.TransferObject;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;

/**
 * Holds child data for a homogeneous join.
 */
public class VirtualJoinTab
{
   // associations

   /**
    * Map of parent OID to a List of composite child TransferObjects.
    */
   protected Lookup m_oidDataMap = new HashTab();

   // operations

   /**
    * Adds children to be joined to a parent.
    * @param oid The parent OID.
    * @param obj The child object; may be a List or a single TransferObject.
    */
   public void addChildObjects(OID oid, Object obj)
   {
      if (obj instanceof List)
      {
         m_oidDataMap.put(oid, obj);
      }
      else if (obj instanceof TransferObject)
      {
         m_oidDataMap.put(oid, Collections.singletonList(obj));
      }
      else
      {
         assert obj == null;
      }
   }

   /**
    * Gets the children for a particular parent.
    * @param oid The OID of the parent.
    * @return The children that will be joined (a List of TransferObjects).
    */
   public List getChildObjects(OID oid)
   {
      return (List)m_oidDataMap.get(oid);
   }

   /**
    * Gets an iterator over the OIDs of parents to which the children
    * will be joined.
    * @return The OID iterator.
    */
   public Iterator getParentOIDIterator()
   {
      return m_oidDataMap.iterator();
   }

   /**
    * Clears the data.
    */
   public void clear()
   {
      m_oidDataMap.clear();
   }
}
