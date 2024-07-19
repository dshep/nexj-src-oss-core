// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

import nexj.core.meta.MetadataMarker;
import nexj.core.meta.NamedMetadataObject;

/**
 * Upgrade stage corresponding to one metadata version change.
 */
public abstract class VersionUpgrade extends NamedMetadataObject
{
   // attributes

   /**
    * The previous version upgrade.
    */
   protected VersionUpgrade m_prev;

   /**
    * The next version upgrade.
    */
   protected VersionUpgrade m_next;

   /**
    * The upgrade container.
    */
   protected Upgrade m_upgrade;

   // constructors

   /**
    * Constructs the upgrade.
    * @param sName The upgrade version.
    */
   public VersionUpgrade(String sName)
   {
      super(sName);
   }

   /**
    * Constructs a preexisiting upgrade (for defining the initial metadata).
    */
   public VersionUpgrade()
   {
   }

   // operations

   /**
    * Sets the upgrade container.
    * @param upgrade The upgrade container to set.
    */
   public void setUpgrade(Upgrade upgrade)
   {
      verifyNotReadOnly();
      m_upgrade = upgrade;
   }

   /**
    * @return The upgrade container.
    */
   public Upgrade getUpgrade()
   {
      return m_upgrade;
   }

   /**
    * Sets the previous version upgrade.
    * @param prev The previous version upgrade to set.
    */
   public void setPrev(VersionUpgrade prev)
   {
      verifyNotReadOnly();
      m_prev = prev;
   }

   /**
    * @return The previous version upgrade.
    */
   public VersionUpgrade getPrev()
   {
      return m_prev;
   }

   /**
    * Sets the next version upgrade.
    * @param next The next version upgrade to set.
    */
   public void setNext(VersionUpgrade next)
   {
      verifyNotReadOnly();
      m_next = next;
   }

   /**
    * @return The next version upgrade.
    */
   public VersionUpgrade getNext()
   {
      return m_next;
   }

   /**
    * @return The upgrade state key, or null if the upgrade state is unsupported.
    * Must be the same value on subsequent invocations.
    */
   public abstract Object getStateKey();

   /**
    * @return A new upgrade state instance.
    */
   public abstract UpgradeState createState();

   /**
    * Applies the upgrade to the state.
    * @param state The upgrade state.
    */
   public abstract void apply(UpgradeState state);

   /**
    * Undoes the upgrade from the state.
    * @param state The upgrade state.
    */
   public abstract void undo(UpgradeState state);

   /**
    * @see nexj.core.meta.MetadataObject#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("VersionUpgrade");
      marker.setProperty("upgrade", m_upgrade.getName());
      marker.setProperty("version", m_sName);
   }
}
