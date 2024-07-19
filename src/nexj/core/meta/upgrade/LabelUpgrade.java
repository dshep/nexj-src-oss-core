// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.upgrade;

/**
 * Upgrade just changing the version.
 */
public class LabelUpgrade extends VersionUpgrade
{
   // constructors

   /**
    * Constructs the upgrade.
    * @param sName The version name.
    */
   public LabelUpgrade(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the upgrade.
    */
   public LabelUpgrade()
   {
   }

   // operations
   
   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#createState()
    */
   public UpgradeState createState()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#getStateKey()
    */
   public Object getStateKey()
   {
      return null;
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#apply(nexj.core.meta.upgrade.UpgradeState)
    */
   public void apply(UpgradeState state)
   {
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#undo(nexj.core.meta.upgrade.UpgradeState)
    */
   public void undo(UpgradeState state)
   {
   }
}
