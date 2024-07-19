// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.ContextMetadata;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataLookupException;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.util.ExceptionHolder;


/**
 * Upgrade step for marking the start of support for a given DataSourceAdapter.
 */
public class SupportAdapterStep extends RelationalSchemaUpgradeStep
{
   // associations

   /**
    * The DataSourceAdapter supported as of this step.
    */
   protected DataSourceAdapter m_adapter;

   // operations

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#apply(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void apply(RelationalSchemaUpgradeState state)
   {
      MetadataCompoundValidationException eh = null;

      // check for duplicate starting point for definition of adapter support
      if (state.containsAdapter(m_adapter))
      {
         MetadataValidationException e = new MetadataValidationException(
            "err.meta.upgrade.sql.adapterDuplicateStart", new Object[]{m_adapter.getName()});

         setProperties(e);
         eh = addException(eh, e);
      }
      else
      {
         state.addAdapter(m_adapter, this);
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @return The adapter for which support begins from the current step.
    */
   public DataSourceAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * @param adapter The adapter to begin support for as of this step.
    */
   public void setAdapter(DataSourceAdapter adapter)
   {
      verifyNotReadOnly();

      assert adapter != null;

      m_adapter = adapter;
   }

   /**
    * @see nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep#undo(nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState)
    */
   public void undo(RelationalSchemaUpgradeState state)
   {
      MetadataCompoundValidationException eh = null;

      // check for duplicate definition of adapter support starting point
      if (state.containsAdapter(m_adapter))
      {
         state.removeAdapter(m_adapter, this);
      }
      else
      {
         MetadataValidationException e = new MetadataValidationException(
            "err.meta.upgrade.sql.adapterDuplicateStart", new Object[]{m_adapter.getName()});

         setProperties(e);
         eh = addException(eh, e);
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return "SupportAdapter(name=" + m_adapter + ')';
   }

   /**
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, nexj.core.util.ExceptionHolder)
    */
   public void validate(ContextMetadata metadata, ExceptionHolder warnings)
   {
      super.validate(metadata, warnings);

      if (m_adapter == null)
      {
         throw new MetadataLookupException("err.meta.adapterLookup", new Object[]{m_adapter, null});
      }
   }
}