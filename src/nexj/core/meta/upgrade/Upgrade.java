// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import java.util.Iterator;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.NamedMetadataObject;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.UncheckedException;

/**
 * The metadata upgrade object.
 */
public class Upgrade extends NamedMetadataObject
{
   // associations

   /**
    * The first version upgrade.
    */
   protected VersionUpgrade m_firstVersion;

   /**
    * The last version upgrade.
    */
   protected VersionUpgrade m_lastVersion;

   /**
    * The version upgrade map: VersionUpgrade[String].
    */
   protected Lookup m_versionMap = new HashTab(8);

   /**
    * The root metadata object.
    */
   protected Metadata m_metadata;

   // operations

   /**
    * Constructs the upgrade.
    * @param sName The upgrade name.
    */
   public Upgrade(String sName)
   {
      super(sName);
   }

   /**
    * Sets the root metadata object.
    * @param metadat The root metadata object to set.
    */
   public void setMetadata(Metadata metadata)
   {
      verifyNotReadOnly();
      m_metadata = metadata;
   }

   /**
    * @return The root metadata object.
    */
   public Metadata getMetadata()
   {
      return m_metadata;
   }

   /**
    * @return The first version upgrade.
    */
   public VersionUpgrade getFirstVersion()
   {
      return m_firstVersion;
   }

   /**
    * @return The last version upgrade.
    */
   public VersionUpgrade getLastVersion()
   {
      return m_lastVersion;
   }
   
   /**
    * Adds a new version upgrade to the upgrade.
    * @param version The version upgrade to add.
    * @throws MetadataException if a version upgrade
    * with the same name already exists.
    */
   public void addVersion(VersionUpgrade version)
   {
      verifyNotReadOnly();

      if (m_lastVersion != null && m_lastVersion.getName() != null &&
         version.getName() == null)
      {
         throw new MetadataException("err.meta.upgrade.versionMismatch",
            new Object[]{m_lastVersion.getName(), m_sName});
      }

      if (version.getName() != null)
      {
         Object oldVersion = m_versionMap.put(version.getName(), version);

         if (oldVersion != null)
         {
            m_versionMap.put(version.getName(), oldVersion);

            throw new MetadataException("err.meta.upgrade.versionDup",
               new Object[]{version.getName(), m_sName});
         }
      }

      if (m_lastVersion == null)
      {
         m_firstVersion = m_lastVersion = version;
      }
      else
      {
         m_lastVersion.setNext(version);
         version.setPrev(m_lastVersion);
         m_lastVersion = version;
      }

      version.setUpgrade(this);
   }

   /**
    * Gets a version upgrade by name.
    * @param sName The version upgrade name.
    * @return The version upgrade object.
    * @throws MetadataLookupException if the version upgrade does not exist.
    */
   public VersionUpgrade getVersion(String sName)
   {
      VersionUpgrade version = (VersionUpgrade) m_versionMap.get(sName);

      if (version != null)
      {
         return version;
      }

      throw new MetadataLookupException("err.meta.upgrade.versionLookup", sName, this);
   }

   /**
    * @return The version upgrade count.
    */
   public int getVersionCount()
   {
      return m_versionMap.size();
   }

   /**
    * @return An iterator for the contained version upgrade objects.
    */
   public Iterator getVersionIterator()
   {
      return m_versionMap.valueIterator();
   }

   /**
    * Return the starting state at a given version (not null).
    * @param sVersion The version to get the starting state for (null == starting version).
    * @return The initial upgrade state.
    */
   public static Lookup/*<Object, UpgradeState>*/ getInitialState(VersionUpgrade version)
      throws MetadataException
   {
      assert version != null;

      Lookup stateMap = new HashTab();

      // need to roll back all to validate that all alter/drop steps have associated create steps
      for (VersionUpgrade u = version.getUpgrade().getLastVersion(); u != null; u = u.getPrev())
      {
         u.undo(getState(stateMap, u));
      }

      // validate state initial state to catch alter/drop steps lacking associated create steps
      for (Lookup.Iterator itr = stateMap.valueIterator(); itr.hasNext();)
      {
         ((UpgradeState)itr.next()).start();
      }

      // roll forward to the requested version
      for (VersionUpgrade u = version.getUpgrade().getFirstVersion(); u != version; u = u.getNext())
      {
         u.apply(getState(stateMap, u));
      }

      return stateMap;
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      Lookup stateMap = new HashTab();
      MetadataCompoundValidationException eh = null;

      if (m_lastVersion != null && !m_metadata.getVersion().equals(m_lastVersion.getName()))
      {
         eh = new MetadataCompoundValidationException();

         MetadataValidationException e = new MetadataValidationException("err.upgrade.lastVersion",
            new Object[]{m_lastVersion.getName(), m_metadata.getVersion()});

         setProperties(e);
         eh.addException(e);
      }

      // roll back the state and validate that can undo each step and state is still valid
      for (VersionUpgrade version = m_lastVersion; version != null; version = version.getPrev())
      {
         try
         {
            version.undo(getState(stateMap, version));
         }
         catch (UncheckedException e)
         {
            eh = version.addException(eh, e);
         }
      }

      if (eh != null)
      {
         throw eh;
      }

      // validate initial state to catch alter/drop steps lacking associated create steps
      for (Lookup.Iterator itr = stateMap.valueIterator(); itr.hasNext();)
      {
         ((UpgradeState)itr.next()).start();
      }

      // validate that the overall state is still valid after every upgrade version is applied
      for (VersionUpgrade version = m_firstVersion; version != null; version = version.getNext())
      {
         try
         {
            version.apply(getState(stateMap, version));
         }
         catch (UncheckedException e)
         {
            eh = version.addException(eh, e);
         }
      }

      if (eh != null)
      {
         throw eh;
      }

      for (Lookup.Iterator itr = stateMap.valueIterator(); itr.hasNext();)
      {
         ((UpgradeState)itr.next()).end();
      }
   }

   /**
    * Gets the upgrade state from a map, lazy-creating it if needed and updating the map.
    * @param stateMap The upgrade state map.
    * @param version The version upgrade.
    */
   public static UpgradeState getState(Lookup stateMap, VersionUpgrade version)
   {
      Object key = version.getStateKey();
      UpgradeState state;

      if (key != null)
      {
         state = (UpgradeState)stateMap.get(key);

         if (state == null)
         {
            state = version.createState();

            if (state != null)
            {
               stateMap.put(key, state);
            }
         }
      }
      else
      {
         state = null;
      }

      return state;
   }

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("Upgrade");
      marker.setProperty("upgrade", m_sName);
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "Upgrade for " + m_metadata;
   }
}
