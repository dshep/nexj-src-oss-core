// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import nexj.core.util.HashHolder;

/**
 * Represents a hierarchical group of primitive privileges and privilege groups.
 */
public final class PrivilegeGroup extends Privilege
{
   // associations
   
   /**
    * The privilege set.
    */
   protected PrivilegeSet m_privilegeSet;

   /**
    * The privilege collection.
    */
   private List m_privilegeList = new ArrayList(8); // of type Privilege

   // constructors.

   /**
    * Constructs the privilege group.
    * @param sName The name of the group.
    */
   public PrivilegeGroup(String sName)
   {
      super(sName);
   }

   // operations

   /**
    * @see nexj.core.meta.Privilege#isPrimitive()
    */
   public boolean isPrimitive()
   {
      return false;
   }

   /**
    * @see nexj.core.meta.Privilege#addTo(nexj.core.meta.PrivilegeSet)
    */
   public void addTo(PrivilegeSet privilegeSet)
   {
      privilegeSet.addAll(m_privilegeSet);
   }
   
   /**
    * Adds a new privilege to the privilege group.
    * @param privilege The privilege to add.
    */
   public void addPrivilege(Privilege privilege)
   {
      verifyNotReadOnly();
      m_privilegeList.add(privilege);
   }

   /**
    * Gets a privilege by ordinal number.
    * @param nOrdinal The privilege ordinal number (0-based).
    * @return The privilege object.
    */
   public Privilege getPrivilege(int nOrdinal)
   {
      return (Privilege)m_privilegeList.get(nOrdinal);
   }

   /**
    * @return The privilege count.
    */
   public int getPrivilegeCount()
   {
      return m_privilegeList.size();
   }

   /**
    * @return An iterator for the contained privilege objects.
    */
   public Iterator getPrivilegeIterator()
   {
      return m_privilegeList.iterator();
   }

   /**
    * Resolves the privilege group.
    * @param metadata The root metadata object.
    * @param activeSet The set of currently resolved groups.
    * @throws MetadataValidationException if a dependency cycle occurs.
    */
   public void resolve(Metadata metadata, Set activeSet) throws MetadataValidationException
   {
      verifyNotReadOnly();
      
      if (m_privilegeSet == null)
      {
         if (!activeSet.add(this))
         {
            MetadataValidationException e = new MetadataValidationException(
               "err.meta.privilegeDepCycle", new Object[]{getName()});

            setProperties(e);

            throw e;
         }

         m_privilegeSet = metadata.createPrivilegeSet();

         for (Iterator itr = getPrivilegeIterator(); itr.hasNext();)
         {
            Privilege privilege = (Privilege)itr.next();

            if (privilege instanceof PrivilegeGroup)
            {
               ((PrivilegeGroup)privilege).resolve(metadata, activeSet);
            }

            privilege.addTo(m_privilegeSet);
         }
         
         activeSet.remove(this);
      }
   }

   /**
    * Resolves all the privileges groups.
    * @param metadata The root metadata object.
    * @throws MetadataValidationException if a dependency cycle occurs.
    */
   public static void resolve(Metadata metadata) throws MetadataValidationException
   {
      Set activeSet = new HashHolder();

      for (Iterator itr = metadata.getPrivilegeIterator(); itr.hasNext();)
      {
         Privilege privilege = (Privilege)itr.next();

         if (privilege instanceof PrivilegeGroup)
         {
            ((PrivilegeGroup)privilege).resolve(metadata, activeSet);
         }
      }
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("PrivilegeGroup");
      marker.setProperty("privilegeGroup", m_sName);
   }
}
