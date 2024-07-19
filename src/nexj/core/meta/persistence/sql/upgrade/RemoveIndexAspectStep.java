// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.MetadataException;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalSchema;

/**
 * Upgrade step for removing an index aspect from matching indexes.
 */
public class RemoveIndexAspectStep extends IndexAspectUpgradeStep
{
   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      RelationalSchema schema = state.getSchema();

      m_index = schema.getIndex(m_sAspectName);

      if (m_index.getType() != Index.ASPECT)
      {
         throw new MetadataException("err.meta.indexAspectUpgradeType", new Object[]{m_sAspectName});
      }

      new IndexAspectUpgradeManager(schema).removeAspects();
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
   }
}
