// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.Aspect;
import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLObject;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.util.HashTab;
import nexj.core.util.Lookup;
import nexj.core.util.ObjUtil;
import nexj.core.util.UncheckedException;
import nexj.core.util.Undefined;

/**
 * Relational schema upgrade state.
 * Keeps track of phantom objects (i.e. objects that should have existed
 * in a previous metadata) during the undo phase.
 */
public class RelationalSchemaUpgradeState implements UpgradeState
{
   // associations

   /**
    * A cached instance of the valid adapters (lazy init and reset on adapter increase/decrease).
    */
   protected DataSourceAdapter[] m_adapterArray;

   /**
    * The upgraded schema.
    */
   protected RelationalSchema m_schema;

   /**
    * The final schema.
    */
   protected RelationalSchema m_final;

   /**
    * The upgrade metadata object.
    */
   protected Upgrade m_upgrade;

   /**
    * The phantom adapter to upgrade step map, i.e. DataSourceAdapter not supported by state.
    * value is either RelationalSchemaUpgradeStep, Undefined.VALUE or null
    * value == the step at which the adapter became no longer supported (i.e. now not supported)
    * null value == the adapter was first removed and then added back (i.e. now supported)
    */
   protected Lookup/*<DataSourceAdapter, Object>*/ m_adapterMap =
      new HashTab/*<DataSourceAdapter, Object>*/();

   /**
    * The phantom table name to upgrade step map: RelationalSchemaUpgradeStep[String].
    */
   protected Lookup m_tableMap = new HashTab();

   /**
    * The table name to column name to column upgrade info map: ColumnUpgradeInfo[String][String]. 
    */
   protected Lookup m_columnMap = new HashTab();

   /**
    * The phantom index name to upgrade step map: RelationalSchemaUpgradeStep[String].
    */
   protected Lookup m_indexMap = new HashTab();

   /**
    * The table aspect name to table aspect step list map: TableAspectStep[][String]. 
    */
   protected Lookup m_aspectMap = new HashTab();

   /**
    * A map of names of phantom SQL Objects (SQLObjects that exist in the upgrade versions but have
    * been dropped prior to the final schema version) to the step where they were dropped.
    */
   protected Lookup/*<String, RelationalSchemaUpgradeStep>*/ m_objectMap =
      new HashTab/*<String, RelationalSchemaUpgradeStep>*/();

   // constructors

   /**
    * Constructs the upgrade state.
    * @param schema The upgraded schema. Must be modifiable.
    * @param result The final schema.
    * @param upgrade The upgrade metadata.
    */
   public RelationalSchemaUpgradeState(RelationalSchema schema, RelationalSchema result, Upgrade upgrade)
   {
      m_schema = schema;
      m_final = result;
      m_upgrade = upgrade;
   }

   // operations

   /**
    * Add the requested adapter to the supported list.
    * @param adapter The adapter which is supported.
    * @param step The upgrade step which caused this adapter to be added.
    */
   public void addAdapter(DataSourceAdapter adapter, RelationalSchemaUpgradeStep step)
   {
      if (m_adapterMap.get(adapter) == step)
      {
         m_adapterArray = null; // invalidate cached list
         m_adapterMap.put(adapter, null); // mark adapter as compatible
      }
   }

   /**
    * Checks if the requiested adapter is supported.
    * @param adapter The adapter to check.
    * @return True if the adapter is supported.
    */
   public boolean containsAdapter(DataSourceAdapter adapter)
   {
      return m_adapterMap.get(adapter) == null; // adapter currently not disabled by any step
   }

   /**
    * Return an array of supported adapters for the current state of the schema (do not modify).
    */
   public DataSourceAdapter[] getAdapters()
   {
      if (m_adapterArray == null)
      {
         List/*<DataSourceAdapter>*/ adapterList =
            new ArrayList/*<DataSourceAdapter>*/(m_adapterMap.size());

         // build a list of DataSourceAdapter supported by this state
         for (Iterator/*<DataSourceAdapter>*/ itr =
                 m_schema.getDataSource().getType().getAdapterIterator();
              itr.hasNext();)
         {
            DataSourceAdapter adapter = (DataSourceAdapter)itr.next();

            if (containsAdapter(adapter))
            {
               adapterList.add(adapter);
            }
         }

         m_adapterArray =
            (DataSourceAdapter[])adapterList.toArray(new DataSourceAdapter[adapterList.size()]);
      }

      return m_adapterArray;
   }

   /**
    * Remove the requested adapter from the supported list.
    * @param adapter The adapter which is no longer supported.
    * @param step The upgrade step which caused this adapter to be removed.
    */
   public void removeAdapter(DataSourceAdapter adapter, RelationalSchemaUpgradeStep step)
   {
      if (!m_adapterMap.contains(adapter))
      {
         m_adapterArray = null; // invalidate cached list
         m_adapterMap.put(adapter, step); // mark adapter as removed
      }
   }

   /**
    * Adds a phantom SQL Object to the state
    * @param sName The name of the phantom SQL Object.
    * @param dropObjectStep The upgrade step which caused the SQL Object to be dropped.
    */
   public void addObject(String sName, DropObjectStep step)
   {
      m_objectMap.put(sName, step);
   }

   /**
    * Remove a phantom table from the state.
    * @param sName The name of the phantom SQL Object.
    * @return True if the SQL Object was found and removed.
    */
   public boolean removeObject(String sName)
   {
      return m_objectMap.remove(sName) != null;
   }

   /**
    * Adds a phantom table to the state.
    * @param sName The table name.
    * @param step The upgrade step.
    */
   public void addTable(String sName, RelationalSchemaUpgradeStep step)
   {
      m_tableMap.put(sName, step);
   }

   /**
    * Removes a phantom table from the state.
    * @param sName The table name.
    * @return True if the table was found and removed.
    */
   public boolean removeTable(String sName)
   {
      m_columnMap.remove(sName);

      return m_tableMap.remove(sName) != null;
   }

   /**
    * Renames a phantom table in the state.
    * @param sOldName The old table name.
    * @param sNewName The new table name.
    */
   public boolean renameTable(String sOldName, String sNewName)
   {
      Object obj = m_columnMap.remove(sOldName);

      if (obj != null)
      {
         m_columnMap.put(sNewName, obj);
      }

      obj = m_tableMap.remove(sOldName);

      if (obj == null)
      {
         return false;
      }

      m_tableMap.put(sNewName, obj);

      return true;
   }

   /**
    * Checks if a phantom table exists.
    * @param sName The table name.
    * @return True if the phantom table exists.
    */
   public boolean containsTable(String sName)
   {
      return m_tableMap.contains(sName);
   }

   /**
    * Adds a phantom index to the state.
    * @param sName The index name.
    * @param step The upgrade step.
    */
   public void addIndex(String sName, RelationalSchemaUpgradeStep step)
   {
      m_indexMap.put(sName, step);
   }

   /**
    * Removes a phantom index from the state.
    * @param sName The index name.
    * @return True if the index was found and removed.
    */
   public boolean removeIndex(String sName)
   {
      return m_indexMap.remove(sName) != null;
   }

   /**
    * Renames a phantom index in the state.
    * @param sOldName The old index name.
    * @param sNewName The new index name.
    */
   public boolean renameIndex(String sOldName, String sNewName)
   {
      Object obj = m_indexMap.remove(sOldName);

      if (obj == null)
      {
         return false;
      }

      m_indexMap.put(sNewName, obj);

      return true;
   }

   /**
    * Checks if a phantom index exists.
    * @param sName The index name.
    * @return True if the phantom index exists.
    */
   public boolean containsIndex(String sName)
   {
      return m_indexMap.contains(sName);
   }

   /**
    * Adds a column to the state.
    * @param sTable The table name.
    * @param sName The column name.
    * @param step The upgrade step.
    * @param bPhantom True if the column will be dropped.
    */
   public void addColumn(String sTable, String sName, RelationalSchemaUpgradeStep step, boolean bPhantom)
   {
      Lookup columnMap = (Lookup)m_columnMap.get(sTable);

      if (columnMap == null)
      {
         columnMap = new HashTab();
         m_columnMap.put(sTable, columnMap);
      }

      ColumnUpgradeInfo info = (ColumnUpgradeInfo)columnMap.get(sName);

      if (info != null)
      {
         if (bPhantom && !info.isPhantom())
         {
            info.setPhantom(true);
            info.setStep(step);
         }
      }
      else
      {
         columnMap.put(sName, new ColumnUpgradeInfo(step, bPhantom));
      }
   }

   /**
    * Removes a phantom column from the state.
    * @param sTable The table name.
    * @param sName The column name.
    * @return True if the column was found and removed.
    */
   public boolean removeColumn(String sTable, String sName)
   {
      Lookup columnMap = (Lookup)m_columnMap.get(sTable);

      if (columnMap == null)
      {
         return false;
      }

      ColumnUpgradeInfo info = (ColumnUpgradeInfo)columnMap.remove(sName);

      return info != null && info.isPhantom();
   }

   /**
    * Renames a phantom column in the state.
    * @param sTable The table name.
    * @param sOldName The old column name.
    * @param sNewName The new column name.
    * @return True if the column was found and renamed.
    */
   public boolean renameColumn(String sTable, String sOldName, String sNewName)
   {
      Lookup columnMap = (Lookup)m_columnMap.get(sTable);

      if (columnMap == null)
      {
         return false;
      }

      ColumnUpgradeInfo info = (ColumnUpgradeInfo)columnMap.remove(sOldName);

      if (info == null)
      {
         return false;
      }

      columnMap.put(sNewName, info);

      return info.isPhantom();
   }

   /**
    * Checks if a phantom column exists.
    * @param sTable The table name.
    * @param sName The column name.
    * @return True if the column exists.
    */
   public boolean containsColumn(String sTable, String sName)
   {
      Lookup columnMap = (Lookup)m_columnMap.get(sTable);

      if (columnMap == null)
      {
         return false;
      }

      ColumnUpgradeInfo info = (ColumnUpgradeInfo)columnMap.get(sName);

      return info != null && info.isPhantom();
   }

   /**
    * Adds a table aspect upgrade step.
    * @param step The upgrade step.
    */
   public void addTableAspect(TableAspectUpgradeStep step)
   {
      List list = (List)m_aspectMap.get(step.getAspectName());

      if (list == null)
      {
         list = new ArrayList();
         m_aspectMap.put(step.getAspectName(), list);
      }

      list.add(step);
   }

   /**
    * Removes a table aspect.
    * @param sName The aspect name.
    * @return True if the aspect was found and removed.
    */
   public boolean removeTableAspect(String sName)
   {
      return m_aspectMap.remove(sName) != null;
   }

   /**
    * Undoes matching table aspects.
    * @param pointcut The pointcut table.
    */
   public void undoTableAspects(Table pointcut)
   {
      for (Iterator itr = m_aspectMap.valueIterator(); itr.hasNext();)
      {
         List list = (List)itr.next();

         for (int i = list.size() - 1; i >= 0; --i)
         {
            ((TableAspectUpgradeStep)list.get(i)).undo(pointcut);
         }
      }
   }

   /**
    * @see nexj.core.meta.upgrade.UpgradeState#start()
    */
   public void start() throws MetadataException
   {
      // Validate the initial state

      MetadataCompoundValidationException eh = null;

      // ensure SQL Objects that were deleted in the upgrade steps still exist in initial schema
      for (Lookup.Iterator itr = m_objectMap.iterator(); itr.hasNext();)
      {
         try
         {
            m_schema.getObject((String)itr.next());
         }
         catch (UncheckedException e)
         {
            eh = ((RelationalSchemaUpgradeStep)itr.getValue()).addException(eh, e);
         }
      }

      for (Lookup.Iterator itr = m_tableMap.iterator(); itr.hasNext();)
      {
         try
         {
            m_schema.getTable((String)itr.next());
         }
         catch (UncheckedException e)
         {
            eh = ((RelationalSchemaUpgradeStep)itr.getValue()).addException(eh, e);
         }
      }

      for (Lookup.Iterator itr = m_indexMap.iterator(); itr.hasNext();)
      {
         try
         {
            m_schema.getIndex((String)itr.next());
         }
         catch (UncheckedException e)
         {
            eh = ((RelationalSchemaUpgradeStep)itr.getValue()).addException(eh, e);
         }
      }
      
      for (Lookup.Iterator tblItr = m_columnMap.valueIterator(); tblItr.hasNext();)
      {
         Lookup columnMap = (Lookup)tblItr.next();

         try
         {
            Table table = m_schema.getTable((String)tblItr.getKey());

            for (Lookup.Iterator colItr = columnMap.iterator(); colItr.hasNext();)
            {
               String sName = (String)colItr.next();
               ColumnUpgradeInfo info = (ColumnUpgradeInfo)colItr.getValue();
               
               try
               {
                  Column column = table.getColumn(sName);

                  if (!info.isPhantom())
                  {
                     throw new MetadataException("err.meta.upgrade.sql.missingColumnDef",
                        new Object[]{column.getName(), table.getName(), m_final.getDataSource().getName()});
                  }
               }
               catch (UncheckedException e)
               {
                  eh = info.getStep().addException(eh, e);
               }
            }
         }
         catch (UncheckedException e)
         {
            for (Lookup.Iterator/*<String, ColumnUpgradeInfo>*/ colItr = columnMap.valueIterator();
                 colItr.hasNext();)
            {
               eh = ((ColumnUpgradeInfo)colItr.next()).getStep().addException(eh, e);
            }
         }
      }

      DataSourceAdapter currentAdapter = m_schema.getDataSource().getAdapter();

      // set all adapters lacking explicit support as unsupported unless its the current adapter
      // i.e. fallback to original validation behaviour
      for (Iterator/*<DataSourceAdapter>*/ itr =
              m_schema.getDataSource().getType().getAdapterIterator();
           itr.hasNext();)
      {
         Object adapter = itr.next();

         if (!m_adapterMap.contains(adapter))
         {
            m_adapterMap.put(adapter, (adapter == currentAdapter) ? null : Undefined.VALUE);
         }
      }

      m_adapterArray = null; // invalidate cached list
      m_tableMap = null;
      m_indexMap = null;
      m_columnMap = null;
      m_aspectMap = null;

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @see nexj.core.meta.upgrade.UpgradeState#end()
    */
   public void end() throws MetadataException
   {
      MetadataCompoundValidationException eh = null;

      // Validate the final state

      // there should be no unsupported adapters left at the end of an upgrade (algorithm error)
      for (Iterator itr = m_adapterMap.valueIterator(); itr.hasNext();)
      {
         Object step = itr.next();

         assert step == null || step == Undefined.VALUE; //all adapters valid or disabled by start()
      }

      // ensure all SQLObjects in DataSource exist after rolling forward through the upgrades
      for (Iterator itr = m_schema.getObjectIterator(); itr.hasNext();)
      {
         SQLObject obj = (SQLObject)itr.next();

         if (m_final.findObject(obj.getName()) == null)
         {
            eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraObject",
               new Object[]{obj.getName(), m_final.getDataSource().getName()}));
         }
      }

      for (Iterator tableItr = m_schema.getTableIterator(); tableItr.hasNext();)
      {
         Table table = (Table)tableItr.next();

         if (m_final.findTable(table.getName()) == null)
         {
            eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraTable",
               new Object[]{table.getName(), m_final.getDataSource().getName()}));
         }
      }

      for (Iterator tableItrFinal = m_final.getTableIterator(); tableItrFinal.hasNext();)
      {
         Table tableFinal = (Table)tableItrFinal.next();
         Table table = m_schema.findTable(tableFinal.getName());

         if (table == null)
         {
            eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingTable",
               new Object[]{tableFinal.getName(), m_final.getDataSource().getName()}));
         }
         else
         {
            if (!ObjUtil.equal(tableFinal.getAlias(), table.getAlias()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableAliasMismatch",
                  new Object[]{tableFinal.getAlias(), table.getAlias(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (!ObjUtil.equal(tableFinal.getOwnerName(), table.getOwnerName()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableOwnerMismatch",
                  new Object[]{tableFinal.getOwnerName(), table.getOwnerName(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (tableFinal.getType() != table.getType())
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableTypeMismatch",
                  new Object[]{tableFinal.getTypeString(), table.getTypeString(), tableFinal.getName(),
                     m_final.getDataSource().getName()}));
            }

            if (m_schema.getMetadata().isPrimaryKeyUpgradeValidationEnabled() &&
                ((tableFinal.getPrimaryKey() == null) != (table.getPrimaryKey() == null) ||
                 tableFinal.getPrimaryKey() != null &&
                 !ObjUtil.equal(
                    tableFinal.getPrimaryKey().getName(), table.getPrimaryKey().getName())))
            {
               eh = addException(
                  eh, new MetadataException("err.meta.upgrade.sql.tablePrimaryKeyMismatch",
                  new Object[]{
                     tableFinal.getPrimaryKey(), table.getPrimaryKey(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (table.getType() == Table.QUERY || table.getType() == Table.VIEW)
            {
               // ensure view SQL scripts are identical
               if (!table.getViewScript().equals(tableFinal.getViewScript()))
               {
                  eh = addException(
                     eh,
                     new MetadataException(
                        "err.meta.upgrade.sql.viewTableMismatch",
                        new Object[]{table.getName(), m_final.getDataSource().getName()}));
               }

               if (table.isViewAutoUpdated() != tableFinal.isViewAutoUpdated())
               {
                  eh = addException(
                    eh,
                    new MetadataException(
                       "err.meta.upgrade.sql.viewUpdatedMismatch",
                       new Object[]{table.getName(), m_final.getDataSource().getName()}));
               }
            }

            // validate disabled hints
            for (Iterator/*<String>*/ hintItr = table.getHintIterator(); hintItr.hasNext();)
            {
               Object hint = hintItr.next();

               if (!tableFinal.isHintEnabled(hint.toString()))
               {
                  eh = addException(
                     eh,
                     new MetadataException("err.meta.upgrade.sql.tableHintMismatch",
                     new Object[]{
                        hint, Boolean.FALSE, table.getName(), m_final.getDataSource().getName()}));
               }
            }

            // validate enabled hints
            for (Iterator/*<String>*/ hintItr = tableFinal.getHintIterator(); hintItr.hasNext();)
            {
               Object hint = hintItr.next();

               if (!table.isHintEnabled(hint.toString()))
               {
                  eh = addException(
                     eh,
                     new MetadataException("err.meta.upgrade.sql.tableHintMismatch",
                     new Object[]{
                        hint, Boolean.TRUE, table.getName(), m_final.getDataSource().getName()}));
               }
            }

            if (!ObjUtil.equal(tableFinal.getTablespaceName(), table.getTablespaceName()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableTablespaceMismatch",
                  new Object[]{(tableFinal.getTablespaceName() == null) ? "" : tableFinal.getTablespaceName(),
                     (table.getTablespaceName() == null) ? "" : table.getTablespaceName(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (!ObjUtil.equal(tableFinal.getIndexspaceName(), table.getIndexspaceName()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableIndexspaceMismatch",
                  new Object[]{(tableFinal.getIndexspaceName() == null) ? "" : tableFinal.getIndexspaceName(),
                     (table.getIndexspaceName() == null) ? "" : table.getIndexspaceName(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (!ObjUtil.equal(tableFinal.getLongspaceName(), table.getLongspaceName()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.tableLongspaceMismatch",
                  new Object[]{(tableFinal.getLongspaceName() == null) ? "" : tableFinal.getLongspaceName(),
                     (table.getLongspaceName() == null) ? "" : table.getLongspaceName(),
                     tableFinal.getName(), m_final.getDataSource().getName()}));
            }

            for (int i = 0, n = table.getAspectCount(); i < n; ++i)
            {
               Aspect aspect = m_final.findTable(table.getAspect(i).getName());

               if (aspect != null && !tableFinal.hasAspect(aspect))
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraTableAspect",
                     new Object[]{aspect.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
               }
            }

            for (int i = 0, n = tableFinal.getAspectCount(); i < n; ++i)
            {
               Aspect aspect = m_schema.findTable(tableFinal.getAspect(i).getName());

               if (aspect != null && !table.hasAspect(aspect))
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingTableAspect",
                     new Object[]{aspect.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
               }
            }

            for (int i = 0, n = table.getColumnCount(); i < n; ++i)
            {
               Column column = table.getColumn(i);

               if (tableFinal.findColumn(column.getName()) == null)
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraColumn",
                     new Object[]{column.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
               }
            }

            for (int i = 0, n = tableFinal.getColumnCount(); i < n; ++i)
            {
               Column columnFinal = tableFinal.getColumn(i);
               Column column = table.findColumn(columnFinal.getName());

               if (column == null)
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingColumn",
                     new Object[]{columnFinal.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
               }
               else
               {
                  if (columnFinal.getType() != column.getType())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnTypeMismatch",
                        new Object[]{columnFinal.getType().getName(), column.getType().getName(),
                           columnFinal.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  if (columnFinal.getPrecision() != column.getPrecision())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnPrecisionMismatch",
                        new Object[]{Primitive.createInteger(columnFinal.getPrecision()),
                           Primitive.createInteger(column.getPrecision()), columnFinal.getName(),
                           tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  if (columnFinal.getScale() != column.getScale())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnScaleMismatch",
                        new Object[]{Primitive.createInteger(columnFinal.getScale()),
                           Primitive.createInteger(column.getScale()), columnFinal.getName(),
                           tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  if (columnFinal.getAllocation() != column.getAllocation())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnAllocationMismatch",
                        new Object[]{columnFinal.getAllocationString(), column.getAllocationString(),
                           columnFinal.getName(), tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  if (columnFinal.isNullable() != column.isNullable())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnNullabilityMismatch",
                        new Object[]{Boolean.valueOf(columnFinal.isNullable()),
                           Boolean.valueOf(column.isNullable()), columnFinal.getName(),
                           tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  if (columnFinal.isCaseInsensitive() != column.isCaseInsensitive())
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnCaseInsensitivityMismatch",
                        new Object[]{Boolean.valueOf(columnFinal.isCaseInsensitive()),
                           Boolean.valueOf(column.isCaseInsensitive()), columnFinal.getName(),
                           tableFinal.getName(), m_final.getDataSource().getName()}));
                  }

                  String sConverterFinal = "";
                  String sConverter = "";

                  if (columnFinal.getConverter() != null)
                  {
                     sConverterFinal = columnFinal.getConverter().getName();
                  }

                  if (column.getConverter() != null)
                  {
                     sConverter = column.getConverter().getName();
                  }

                  if (!sConverterFinal.equals(sConverter))
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.columnConverterMismatch",
                        new Object[]{sConverterFinal, sConverter, columnFinal.getName(),
                           tableFinal.getName(), m_final.getDataSource().getName()}));
                  }
               }
            }
         }
      }

      for (Iterator indexItr = m_schema.getIndexIterator(); indexItr.hasNext();)
      {
         Index index = (Index)indexItr.next();

         if (m_final.findIndex(index.getName()) == null)
         {
            eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraIndex",
               new Object[]{index.getName(), index.getTable().getName(), m_final.getDataSource().getName()}));
         }
      }

      for (Iterator indexItrFinal = m_final.getIndexIterator(); indexItrFinal.hasNext();)
      {
         Index indexFinal = (Index)indexItrFinal.next();
         Index index = m_schema.findIndex(indexFinal.getName());

         if (index == null)
         {
            if (indexFinal.getType() >= Index.BTREE)
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingIndex",
                  new Object[]{indexFinal.getName(), indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
            }
         }
         else
         {
            if (!indexFinal.getTable().getName().equals(index.getTable().getName()))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexTableMismatch",
                  new Object[]{indexFinal.getTable().getName(), index.getTable().getName(),
                     indexFinal.getName(), m_final.getDataSource().getName()}));
            }

            if (indexFinal.getType() != index.getType())
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexTypeMismatch",
                  new Object[]{indexFinal.getTypeString(), index.getTypeString(), indexFinal.getName(),
                     indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
            }

            if (indexFinal.isUnique() != index.isUnique())
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexUniquenessMismatch",
                  new Object[]{Boolean.valueOf(indexFinal.isUnique()), Boolean.valueOf(index.isUnique()),
                     indexFinal.getName(), indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
            }

            if (indexFinal.getFill() != index.getFill())
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexFillMismatch",
                  new Object[]{Primitive.createInteger(indexFinal.getFill()), Primitive.createInteger(index.getFill()),
                     indexFinal.getName(), indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
            }

            String sRelatedTableFinal = (indexFinal.getRelatedTable() == null) ? "" : indexFinal.getRelatedTable().getName();
            String sRelatedTable = (index.getRelatedTable() == null) ? "" : index.getRelatedTable().getName();

            if (!sRelatedTableFinal.equals(sRelatedTable))
            {
               eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexRelatedTableMismatch",
                  new Object[]{sRelatedTableFinal, sRelatedTable, indexFinal.getName(),
                     indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
            }

            for (int i = 0, n = index.getAspectCount(); i < n; ++i)
            {
               Aspect aspect = m_final.findIndex(index.getAspect(i).getName());

               if (aspect != null && !indexFinal.hasAspect(aspect))
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraIndexAspect",
                     new Object[]{aspect.getName(), indexFinal.getName(), indexFinal.getTable().getName(),
                        m_final.getDataSource().getName()}));
               }
            }

            for (int i = 0, n = indexFinal.getAspectCount(); i < n; ++i)
            {
               Aspect aspect = m_schema.findIndex(indexFinal.getAspect(i).getName());

               if (aspect != null && !index.hasAspect(aspect))
               {
                  eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingIndexAspect",
                     new Object[]{aspect.getName(), indexFinal.getName(), indexFinal.getTable().getName(),
                        m_final.getDataSource().getName()}));
               }
            }

            if (index.getType() != Index.VIRTUAL && index.getType() != Index.QUERY)
            {
               for (int i = 0, n = index.getIndexColumnCount(); i < n; ++i)
               {
                  IndexColumn indexColumn = index.getIndexColumn(i);
   
                  if (indexFinal.findIndexColumn(indexColumn.getColumn().getName()) == null)
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.extraIndexColumn",
                        new Object[]{indexColumn.getColumn().getName(), index.getName(),
                           index.getTable().getName(), m_final.getDataSource().getName()}));
                  }
               }

               for (int i = 0, n = indexFinal.getIndexColumnCount(); i < n; ++i)
               {
                  IndexColumn indexColumnFinal = indexFinal.getIndexColumn(i);
                  IndexColumn indexColumn = index.findIndexColumn(indexColumnFinal.getColumn().getName());

                  if (indexColumn == null)
                  {
                     eh = addException(eh, new MetadataException("err.meta.upgrade.sql.missingIndexColumn",
                        new Object[]{indexColumnFinal.getColumn().getName(), indexFinal.getName(),
                           indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
                  }
                  else
                  {
                     if (indexColumnFinal.getOrdinal() != indexColumn.getOrdinal())
                     {
                        eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexColumnOrdinalMismatch",
                           new Object[]{Primitive.createInteger(indexColumnFinal.getOrdinal()),
                              Primitive.createInteger(indexColumn.getOrdinal()), indexColumnFinal.getColumn().getName(),
                              indexFinal.getName(), indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
                     }

                     if (indexColumnFinal.isAscending() != indexColumn.isAscending())
                     {
                        eh = addException(eh, new MetadataException("err.meta.upgrade.sql.indexColumnAscendingMismatch",
                           new Object[]{Boolean.valueOf(indexColumnFinal.isAscending()),
                              Boolean.valueOf(indexColumn.isAscending()), indexColumnFinal.getColumn().getName(),
                              indexFinal.getName(), indexFinal.getTable().getName(), m_final.getDataSource().getName()}));
                     }
                  }
               }
            }
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * Adds an exception to a compound validation exception.
    * @param eh The exception holder, or null if not yet allocated.
    * @param e The exception to add.
    * @return The exception holder.
    */
   protected MetadataCompoundValidationException addException(
      MetadataCompoundValidationException eh, UncheckedException e)
   {
      return m_upgrade.addException(eh, e);
   }

   /**
    * @return The upgraded schema.
    */
   public RelationalSchema getSchema()
   {
      return m_schema;
   }

   // inner classes

   /**
    * Column upgrade information.
    */
   protected static class ColumnUpgradeInfo
   {
      // attributes

      /**
       * The phantom column flag.
       */
      protected boolean m_bPhantom;

      // associations

      /**
       * The upgrade step.
       */
      protected RelationalSchemaUpgradeStep m_step;

      // constructors

      /**
       * Constructs the info object.
       * @param step The upgrade step.
       * @param bPhantom True if the column will be dropped.
       */
      public ColumnUpgradeInfo(RelationalSchemaUpgradeStep step, boolean bPhantom)
      {
         m_step = step;
         m_bPhantom = bPhantom;
      }

      // operations

      /**
       * Sets the upgrade step.
       * @param step The upgrade step to set.
       */
      public void setStep(RelationalSchemaUpgradeStep step)
      {
         m_step = step;
      }

      /**
       * @return The upgrade step.
       */
      public RelationalSchemaUpgradeStep getStep()
      {
         return m_step;
      }
      
      /**
       * Sets the phantom column flag.
       * @param bPhantom The phantom column flag to set.
       */
      public void setPhantom(boolean bPhantom)
      {
         m_bPhantom = bPhantom;
      }

      /**
       * @return The phantom column flag.
       */
      public boolean isPhantom()
      {
         return m_bPhantom;
      }

      /**
       * @see java.lang.Object#toString()
       */
      public String toString()
      {
         return "ColumnInfo(upgrade=" + m_step.getUpgrade().getName() + ", step=" +
            m_step.getUpgrade().findStepOrdinal(m_step) + ", phantom=" + m_bPhantom + ')';
      }
   }
}
