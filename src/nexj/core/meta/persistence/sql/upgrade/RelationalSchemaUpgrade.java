// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataMarker;
import nexj.core.meta.persistence.SchemaUpgrade;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.util.UncheckedException;

/**
 * Relational schema version upgrade. 
 */
public class RelationalSchemaUpgrade extends SchemaUpgrade
{
   // associations

   /**
    * The upgrade step collection.
    */
   protected List m_stepList = new ArrayList(4); // of type RelationalSchemaUpgradeStep

   // constructors

   /**
    * Constructs the version upgrade.
    * @param sName The version.
    */
   public RelationalSchemaUpgrade(String sName)
   {
      super(sName);
   }

   /**
    * Constructs a preexisting version upgrade. 
    */
   public RelationalSchemaUpgrade()
   {
   }

   // operations

   /**
    * Adds a new upgrade step to the version upgrade.
    * @param step The upgrade step to add.
    */
   public void addStep(RelationalSchemaUpgradeStep step)
   {
      verifyNotReadOnly();
      m_stepList.add(step);
      step.setUpgrade(this);
   }

   /**
    * Gets a upgrade step by ordinal number.
    * @param nOrdinal The upgrade step ordinal number (0-based).
    * @return The upgrade step object.
    */
   public RelationalSchemaUpgradeStep getStep(int nOrdinal)
   {
      return (RelationalSchemaUpgradeStep)m_stepList.get(nOrdinal);
   }

   /**
    * Finds a step ordinal number.
    * @param step The step to look for.
    * @return The step ordinal number, or -1 if not found.
    */
   public int findStepOrdinal(RelationalSchemaUpgradeStep step)
   {
      return m_stepList.indexOf(step);
   }
   
   /**
    * @return The upgrade step count.
    */
   public int getStepCount()
   {
      return m_stepList.size();
   }

   /**
    * @return An iterator for the contained upgrade step objects.
    */
   public Iterator getStepIterator()
   {
      return m_stepList.iterator();
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#getStateKey()
    */
   public Object getStateKey()
   {
      return m_dataSource.getSchema();
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#createState()
    */
   public UpgradeState createState()
   {
      RelationalSchema schema = (RelationalSchema)m_dataSource.getSchema();

      return new RelationalSchemaUpgradeState((RelationalSchema)schema.clone(), schema, getUpgrade());
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#apply(nexj.core.meta.upgrade.UpgradeState)
    */
   public void apply(UpgradeState state)
   {
      MetadataCompoundValidationException eh = null;

      for (int i = 0, n = getStepCount(); i < n; ++i)
      {
         try
         {
            getStep(i).apply((RelationalSchemaUpgradeState)state);
         }
         catch (UncheckedException e)
         {
            eh = getStep(i).addException(eh, e);
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#undo(nexj.core.meta.upgrade.UpgradeState)
    */
   public void undo(UpgradeState state)
   {
      MetadataCompoundValidationException eh = null;

      for (int i = getStepCount() - 1; i >= 0; --i)
      {
         RelationalSchemaUpgradeStep step = getStep(i);

         try
         {
            step.undo((RelationalSchemaUpgradeState)state);
         }
         catch (UncheckedException e)
         {
            eh = step.addException(eh, e);
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @see nexj.core.meta.upgrade.VersionUpgrade#setProperties(nexj.core.meta.MetadataMarker)
    */
   public void setProperties(MetadataMarker marker)
   {
      marker.setTypeName("RelationalSchemaUpgrade");
      marker.setProperty("upgrade", m_upgrade.getName());
      marker.setProperty("dataSource", m_dataSource.getName());
   }
}
