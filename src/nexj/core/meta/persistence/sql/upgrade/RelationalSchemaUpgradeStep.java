// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataMarker;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;

/**
 * Single step in a version upgrade.
 */
public abstract class RelationalSchemaUpgradeStep extends MetadataObject
{
   // associations

   /**
    * The containing version upgrade.
    */
   protected RelationalSchemaUpgrade m_upgrade;

   // operations

   /**
    * Sets the containing version upgrade.
    * @param upgrade The containing version upgrade to set.
    */
   public void setUpgrade(RelationalSchemaUpgrade upgrade)
   {
      verifyNotReadOnly();
      m_upgrade = upgrade;
   }

   /**
    * @return The containing version upgrade.
    */
   public RelationalSchemaUpgrade getUpgrade()
   {
      return m_upgrade;
   }

   /**
    * Applies the upgrade to the state.
    * @param state The upgrade state.
    */
   public abstract void apply(RelationalSchemaUpgradeState state);

   /**
    * Undoes the upgrade from the state.
    * @param state The upgrade state.
    */
   public abstract void undo(RelationalSchemaUpgradeState state);

   /**
    * @return The type name of the step.
    */
   protected String getTypeName()
   {
      String sType = getClass().getName();

      sType = sType.substring(sType.lastIndexOf('.') + 1);

      if (sType.endsWith("Step"))
      {
         sType = sType.substring(0, sType.length() - 4);
      }

      return sType;
   }

   /**
    * @see MetadataObject#setProperties(MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName(getTypeName());
      marker.setProperty("upgrade", m_upgrade.getUpgrade().getName());
      marker.setProperty("version", m_upgrade.getName());

      int i = m_upgrade.findStepOrdinal(this);

      if (i >= 0)
      {
         marker.setProperty("step", Primitive.createInteger(i));
      }
   }
}
