// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Types;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import nexj.core.meta.Metadata;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalObject;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLObject;
import nexj.core.meta.persistence.sql.SQLScript;
import nexj.core.meta.persistence.sql.SQLScriptHolder;
import nexj.core.meta.persistence.sql.SQLSubstReader;
import nexj.core.meta.persistence.sql.SQLTemplateSubstReader;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.meta.persistence.sql.upgrade.AlterColumnStep;
import nexj.core.meta.persistence.sql.upgrade.AlterTableStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.ApplyTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.CreateColumnStep;
import nexj.core.meta.persistence.sql.upgrade.CreateIndexStep;
import nexj.core.meta.persistence.sql.upgrade.CreateObjectStep;
import nexj.core.meta.persistence.sql.upgrade.CreateTableStep;
import nexj.core.meta.persistence.sql.upgrade.DropColumnStep;
import nexj.core.meta.persistence.sql.upgrade.DropIndexStep;
import nexj.core.meta.persistence.sql.upgrade.DropObjectStep;
import nexj.core.meta.persistence.sql.upgrade.DropTableStep;
import nexj.core.meta.persistence.sql.upgrade.ExecStep;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgrade;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeState;
import nexj.core.meta.persistence.sql.upgrade.RelationalSchemaUpgradeStep;
import nexj.core.meta.persistence.sql.upgrade.RemoveIndexAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RemoveTableAspectStep;
import nexj.core.meta.persistence.sql.upgrade.RenameColumnStep;
import nexj.core.meta.persistence.sql.upgrade.RenameIndexStep;
import nexj.core.meta.persistence.sql.upgrade.RenameTableStep;
import nexj.core.meta.persistence.sql.upgrade.SupportAdapterStep;
import nexj.core.meta.upgrade.LabelUpgrade;
import nexj.core.meta.upgrade.ScriptUpgrade;
import nexj.core.meta.upgrade.Upgrade;
import nexj.core.meta.upgrade.UpgradeState;
import nexj.core.meta.upgrade.VersionUpgrade;
import nexj.core.persistence.Operator;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.SchemaVersion;
import nexj.core.rpc.TransferObject;
import nexj.core.runtime.ValidationException;
import nexj.core.util.Binary;
import nexj.core.util.HashHolder;
import nexj.core.util.HashTab;
import nexj.core.util.HashTab2D;
import nexj.core.util.IOUtil;
import nexj.core.util.Logger;
import nexj.core.util.Lookup;
import nexj.core.util.Lookup2D;
import nexj.core.util.LookupException;
import nexj.core.util.ObjUtil;
import nexj.core.util.ProgressListener;
import nexj.core.util.PropertyIterator;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;
import nexj.core.util.URLUtil;
import nexj.core.util.XMLUtil;

/**
 * Class for reading, creating and upgrading the database schema.
 */
public abstract class SQLSchemaManager
{
   // constants

   /**
    * Tablespace name designating the system default tablespace.
    */
   protected final static String DEFAULT_TABLESPACE = "default";

   /**
    * The public principal GUID.
    */
   protected final static Binary SYS_PUBLIC_GUID = new Binary(
      new byte[]{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00,
         (byte)0x80, 0x00, (byte)0xBE, (byte)0xEF, 0x00, 0x00, 0x00, 0x0A});

   /**
    * The system user GUID.
    */
   protected final static Binary SYS_USER_GUID = new Binary(
      new byte[]{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00,
         (byte)0x80, 0x00, (byte)0xBE, (byte)0xEF, 0x00, 0x00, 0x00, 0x0C});

   /**
    * Comparator used for sorting Relational Objects
    */
   protected final static Comparator RELATIONAL_OBJECT_COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         RelationalObject leftObj = (RelationalObject)left;
         RelationalObject rightObj = (RelationalObject)right;
         int n = -left.getClass().getCanonicalName().compareTo(right.getClass().getCanonicalName());

         if (n != 0)
         {
            return n;
         }

         return (left instanceof Table) ? TABLE_COMPARATOR.compare(left, right)
                                        : leftObj.compareTo(rightObj);
      }
   };

   /**
    * Comparator used for sorting tables.
    */
   protected final static Comparator TABLE_COMPARATOR = new Comparator()
   {
      public int compare(Object left, Object right)
      {
         Table leftTable = (Table)left;
         Table rightTable = (Table)right;
         int n = leftTable.getType() - rightTable.getType();

         if (n != 0)
         {
            return n;
         }

         String sLeftOwner = leftTable.getOwnerName();
         String sRightOwner = rightTable.getOwnerName();

         if (sLeftOwner == null)
         {
            sLeftOwner = "";
         }

         if (sRightOwner == null)
         {
            sRightOwner = "";
         }

         n = sLeftOwner.compareToIgnoreCase(sRightOwner);

         if (n != 0)
         {
            return n;
         }

         return leftTable.getTableName().compareToIgnoreCase(rightTable.getTableName());
      }
   };

   /**
    * The ending upgrade step, denotes that the version is fully upgraded.
    */
   protected final static int UPGRADE_END_STEP = -1;

   // attributes

   /**
    * The metadata object owner.
    */
   protected String m_sOwner;

   /**
    * The current upgrade step ordinal.
    */
   protected int m_nStep;

   // associations

   /**
    * The persistence adapter.
    */
   protected SQLAdapter m_adapter;

   /**
    * The upgrade step container.
    */
   protected RelationalSchemaUpgrade m_upgrade;

   /**
    * The upgrade state.
    */
   protected RelationalSchemaUpgradeState m_state;

   /**
    * The SQL appender.
    */
   protected SQLAppender m_appender;

   /**
    * The SQL connection.
    */
   protected Connection m_connection;

   /**
    * The class logger.
    */
   protected final static Logger s_logger = Logger.getLogger(SQLSchemaManager.class);

   // constructors

   /**
    * Constructs the schema manager.
    * @param adapter The persistence adapter.
    */
   protected SQLSchemaManager(SQLAdapter adapter)
   {
      m_adapter = adapter;
   }

   // operations

   /**
    * Appends a column name to a string buffer.
    * @param buf The string buffer.
    * @param column The column to append.
    */
   protected void appendColumnName(StringBuffer buf, Column column)
   {
      m_adapter.appendColumn(buf, column);
   }

   /**
    * Appends a column name to a string buffer.
    * @param buf The string buffer.
    * @param column The column to append.
    * @param sSuffix The column name suffix. Can be null.
    */
   protected void appendColumnName(StringBuffer buf, Column column, String sSuffix)
   {
      if (sSuffix != null)
      {
         buf.append(column.getName());
         buf.append(sSuffix);
      }
      else
      {
         m_adapter.appendColumn(buf, column);
      }
   }

   /**
    * Appends a full table name to a string buffer. 
    * @param buf The destination string buffer.
    * @param table The table object.
    */
   protected void appendTableName(StringBuffer buf, Table table)
   {
      m_adapter.appendTable(buf, table, getOwner());
   }

   /**
    * Appends a full table name to a string buffer. 
    * @param buf The destination string buffer.
    * @param table The table object.
    * @param sSuffix Optional table name suffix. Can be null.
    */
   protected void appendTableName(StringBuffer buf, Table table, String sSuffix)
   {
      m_adapter.appendTable(buf, table, getOwner(), sSuffix, true);
   }

   /**
    * Gets the full table name.
    * @param table The table object.
    */
   protected String getTableName(Table table)
   {
      return m_adapter.getTableName(table, getOwner());
   }

   /**
    * Gets the full table name.
    * @param table The table object.
    * @param sSuffix Optional table name suffix. Can be null.
    * @param bQuote True to quote the table name.
    */
   protected String getTableName(Table table, String sSuffix, boolean bQuote)
   {
      return m_adapter.getTableName(table, getOwner(), sSuffix, bQuote);
   }

   /**
    * Add all the dependencies the specified script depends on to the specified collection.
    * Dependencies are added to the collection as many times as they are referenced.
    * @param collection The collection to add script dependencies to.
    * @param script The script to return dependencies for.
    * @param schema The schema to search in for dependencies.
    * @param bRequire All dependencies must exist in schema (false == ignore missing dependencies).
    * @return The collection dependencies were added to.
    */
   protected static Collection addDependencies(
      final Collection/*<Object>*/ collection,
      SQLScript script, RelationalSchema schema, final boolean bRequire)
   {
      if (script == null)
      {
         return collection;
      }

      String sSQL = script.findSQL(schema.getDataSource().getAdapter());

      if (StringUtil.isEmpty(sSQL))
      {
         return collection;
      }

      sSQL = sSQL.replaceAll("--[^\n]*", ""); // strip SQL comments, they are not real dependencies

      SQLSubstReader reader = new SQLSubstReader(new StringReader(sSQL), schema, null)
      {
         protected StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray)
         {
            return buf;
         }

         protected StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField)
         {
            return buf;
         }

         protected StringBuffer appendTSIncrement(
            StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField)
         {
            return buf;
         }

         /**
          * @see nexj.core.meta.persistence.sql.SQLSubstReader#getIndex(java.lang.String)
          */
         protected Index getIndex(String sName)
         {
            Index index = (bRequire) ? super.getIndex(sName) : m_schema.findIndex(sName);

            if (bRequire || index != null)
            {
               collection.add(index);
            }

            return index;
         }

         /**
          * @see nexj.core.meta.persistence.sql.SQLSubstReader#getObject(java.lang.String)
          */
         protected SQLObject getObject(String sName)
         {
            SQLObject object = (bRequire) ? super.getObject(sName) : m_schema.findObject(sName);

            if (bRequire || object != null)
            {
               collection.add(object);
            }

            return object;
         }

         protected String getQuotedTableName(Table table)
         {
            return null;
         }

         protected String getQuotedObjectName(SQLObject object)
         {
            return null;
         }

         protected String getQuotedIndexName(Index index)
         {
            return null;
         }

         protected String getQuotedKeyword(String sName)
         {
            return null;
         }

         protected String getQuotedOwnerName()
         {
            return null;
         }

         protected String getQuotedRoleName()
         {
            return null;
         }

         /**
          * @see nexj.core.meta.persistence.sql.SQLSubstReader#getTable(java.lang.String)
          */
         protected Table getTable(String sName)
         {
            Table table = (bRequire) ? super.getTable(sName) : m_schema.findTable(sName);

            if (bRequire || table != null)
            {
               collection.add(table);
            }

            return table;
         }

         protected boolean isColumnCaseInsensitive(Column column)
         {
            return column.isCaseInsensitive();
         }

         protected String getBinaryLiteral(String sHex)
         {
            return null;
         }

         protected String getNow()
         {
            return null;
         }

         protected String getGUID()
         {
            return null;
         }

         protected String getSysPublicId()
         {
            return null;
         }

         protected String getSysUserId()
         {
            return null;
         }

         protected String getSysUserAlias()
         {
            return null;
         }
      };

      try
      {
         while (reader.read() >= 0);
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }

      return collection;
   }

   /**
    * Add all the prerequisites of specified 'obj' to the specified 'map' if not already present.
    * Then add the 'obj' itself to the map. For every addition the value is nNext++.
    * @param obj The object which to add following and including all its existence prerequisites.
    * @param map The map to add the 'obj' and prerequisites to.
    * @param nNext The value to use for the next addition.
    * @return The value to use for the next addition (i.e. nNext param + # of additions done).
    */
   protected static int addPrerequisites(
      final RelationalObject obj, Lookup/*<RelationalObject, Integer>*/ map, int nNext)
   {
      if (map.contains(obj))
      {
         return nNext; // this object has been processed already (base case)
      }

      List/*<Object>*/ prerequisiteList = new ArrayList/*<Object>*/();

      // collect prerequisites from the object itself
      for (Iterator/*<Object>*/ itr = obj.getPrerequisiteIterator(); itr.hasNext();)
      {
         prerequisiteList.add(itr.next());
      }

      // load additional RelationalObject prerequisites from the create script
      addDependencies(
         prerequisiteList,
         (obj instanceof Table) ? ((Table)obj).getViewScript() : ((SQLObject)obj).getCreateScript(),
         obj.getSchema(),
         true);

      if (!prerequisiteList.isEmpty())
      {
         Collections.sort(prerequisiteList, RELATIONAL_OBJECT_COMPARATOR); // set predictable order
         map.put(obj, null); // in the case of loop this will ensure termination

         // go through all existence prerequisites and ensure they are all positioned prior to it
         for (int i = 0, nCount = prerequisiteList.size(); i < nCount; ++i)
         {
            Object prerequisite = prerequisiteList.get(i);

            if (prerequisite instanceof RelationalObject) // may be Index added by addDependencies()
            {
               nNext = addPrerequisites((RelationalObject)prerequisite, map, nNext);
            }
         }
      }

      map.put(obj, Primitive.createInteger(nNext++)); // note new position of object

      return nNext;
   }

   /**
    * Alter an index definition.
    * @param target The modified index definition that the index must match after recreation.
    *               The table of this index must have a valid schema.
    * @param current The original index definition currently persisted in RDBMS, with current table.
    */
   protected void alterIndex(Index target, Index current)
   {
      dropIndex(target);
      createIndex(target);
   }

   /**
    * Concatenate all values in the list with valid SQL syntax.
    * @param buf The destination string buffer (not null).
    * @param argArray The values to concatenate.
    * @return The The destination buffer.
    */
   protected abstract StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray);

   /**
    * Append the index definition, i.e. the part after "create index " containing table and columns.
    * @param buf The destination string buffer (not null).
    * @param index The index object being created.
    * @return The The destination buffer.
    */
   protected StringBuffer appendIndexDefinition(StringBuffer buf, Index index)
   {
      buf.append(" on ");
      appendTableName(buf, index.getTable());
      appendIndexColumns(buf, index);

      return buf;
   }

   /**
    * Suffix to append to a "drop index" statement after the index name.
    * @param buf The destination string buffer (not null).
    * @param index The index being dropped.
    * @return The destination buffer.
    */
   protected StringBuffer appendDropIndexSuffix(StringBuffer buf, Index index)
   {
      return buf;
   }

   /**
    * Extract nField from sTS with valid SQL syntax.
    * @param buf The destination string buffer (not null).
    * @param sTS The timestamp value.
    * @param nField One of SQLSubstReader.TS_* constants.
    * @return The The destination buffer.
    */
   protected abstract StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField);

   /**
    * Increment sTS by sDelta with valid SQL syntax.
    * @param buf The destination string buffer (not null).
    * @param sTS The timestamp value.
    * @param sDelta The number of units to alter the value in the calculation.
    * @param nField One of SQLSubstReader.TS_* constants representing units of sDelta.
    * @return The destination buffer.
    */
   protected abstract StringBuffer appendTSIncrement(
      StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField);

   /**
    * Appends a table view declaration to a string buffer.
    * This method should be overridden by all adapters that support Table.QUERY type views.
    * @param buf The destination string buffer.
    * @param view The view to declare.
    */
   protected void appendViewDeclaration(StringBuffer buf, Table view)
   {
      buf.append("create view ");
      appendTableName(buf, view);
      buf.append(" as ");
      buf.append(getExecSQL(view.getViewScript(), view.getSchema(), view));
   }

   /**
    * Appends SQL for creating an SQL Object to the SQL appender and upgrades the metadata.
    * @param step The step holding the definition of the object to create.
    */
   protected void createObject(CreateObjectStep step)
   {
      step.apply(m_state);
      createObject(step.getObject());
   }

   /**
    * Appends the SQL statements to create a given SQL Object to the SQL appender.
    * @param obj The SQL Object to create.
    */
   protected void createObject(SQLObject obj)
   {
      String sSQL = getExecSQL(obj.getCreateScript(), obj.getSchema(), null);

      if (!StringUtil.isEmpty(sSQL))
      {
         m_appender.appendSQL(sSQL);
      }
   }

   /**
    * Appends SQL for dropping an SQL Object to the SQL appender and upgrades the metadata.
    * @param step The drop table step.
    */
   protected void dropObject(DropObjectStep step)
   {
      step.apply(m_state);
      dropObject(step.getObject());
   }

   /**
    * Appends the SQL statements to drop a given SQL Object to the SQL appender.
    * @param obj The SQL Object to create.
    */
   protected void dropObject(SQLObject obj)
   {
      String sSQL = getExecSQL(obj.getDropScript(), obj.getSchema(), null);

      if (!StringUtil.isEmpty(sSQL))
      {
         m_appender.appendSQL(sSQL);
      }
   }

   /**
    * Find the first version fully compatible with the DataSourceAdapter in the specified state and
    * advance the state to the version.
    * @param version The version to start searching from (not null).
    * @param stateMap The map of states to reference and to advance (not null).
    * @param rdb The RelationalDatabase datasource to match (not null).
    * @return The version compatible with the current adapter (stateMap updated to reflect version).
    */
   public static VersionUpgrade findFirstVersion(
      VersionUpgrade version, Lookup/*<Object, UpgradeState>*/ stateMap, RelationalDatabase rdb)
   {
      assert version != null && stateMap != null;

      for (;version != null; version = version.getNext())
      {
         UpgradeState state = Upgrade.getState(stateMap, version);

         // check for adapter compatibility limiting just to the requested datasource
         if (version instanceof RelationalSchemaUpgrade &&
             ((RelationalSchemaUpgrade)version).getDataSource() == rdb)
         {
            RelationalSchemaUpgrade upgradeVersion = (RelationalSchemaUpgrade)version;
            DataSourceAdapter adapter = upgradeVersion.getDataSource().getAdapter();

            // state already supports adapter
            if (((RelationalSchemaUpgradeState)state).containsAdapter(adapter))
            {
               return version;
            }

            // step through each step, if the AdapterStep enabling the specific adapter is at the
            // beginning of the step list or only has other AdapterSteps before it then this version
            // is compatible
            for (int i = 0, nCount = upgradeVersion.getStepCount(); i < nCount; ++i)
            {
               RelationalSchemaUpgradeStep step = upgradeVersion.getStep(i);

               if (step instanceof SupportAdapterStep)
               {
                  if (((SupportAdapterStep)step).getAdapter() == adapter)
                  {
                     return version;
                  }
               }
               else
               {
                  break;//non-AdapterStep, i.e. some steps support requested Adapter and some do not
               }
            }
         }

         version.apply(state);
      }

      return version;
   }

   /**
    * Get a list of tables the specified script depends on.
    * @param script The script to return dependencies for.
    * @param schema The schema to search in for dependencies.
    * @return An array of tables from the specified schema that the script depends on.
    */
   protected Table[] getDependency(SQLScript script, RelationalSchema schema)
   {
      Set/*<Object>*/ set = new HashHolder/*<Object>*/();

      for (Iterator/*<Object>*/ itr = addDependencies(set, script, schema, false).iterator();
           itr.hasNext();)
      {
         if (!(itr.next() instanceof Table))
         {
            itr.remove();
         }
      }

      return (Table[])set.toArray(new Table[set.size()]);
   }

   /**
    * Get a list of tables that depend on the specified table.
    * @param table The table to return dependents for.
    * @return An array of tables that depend in specified table.
    */
   protected Table[] getInverseDependency(Table table)
   {
      String sMatch = "${table:" + table.getName() + "}";
      List/*<Table>*/ list = new ArrayList/*<Table>*/();

      for (Iterator/*<Table>*/ itr = table.getSchema().getTableIterator(); itr.hasNext();)
      {
         Table view = (Table)itr.next();

         if (view.getType() != Table.QUERY && view.getType() != Table.VIEW || view == table)
         {
            continue; // only Table.QUERY and Table.VIEW can have dependencies
         }

         String sSQL = view.getViewScript().findSQL(view.getSchema().getDataSource().getAdapter());

         if (sSQL != null && sSQL.replaceAll("--[^\n]*\n", "\n").contains(sMatch)) // strip comments
         {
            list.add(view);
         }
      }

      return (Table[])list.toArray(new Table[list.size()]);
   }

   /**
    * Gets a sorted array of Relational Objects from a schema.
    * @param schema The relational schema.
    * @return The sorted array of tables;
    */
   protected static RelationalObject[] getSortedRelationalObjects(
      RelationalSchema schema)
   {
      RelationalObject[] objArray =
         new RelationalObject[schema.getObjectCount() + schema.getTableCount()];
      int i = 0;

      for (Iterator itr = schema.getObjectIterator(); itr.hasNext();)
      {
         objArray[i++] = (RelationalObject)itr.next();
      }

      for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
      {
         objArray[i++] = (RelationalObject)itr.next();
      }

      Arrays.sort(objArray, RELATIONAL_OBJECT_COMPARATOR);

      Lookup/*<RelationalObject, Integer>*/ orderMap =
         new HashTab/*<RelationalObject, Integer>*/(objArray.length);
      int k = 0;

      // reorder based on existence prerequisites, i.e. for every object check its prerequisites and
      // ensure they are positioned prior to this object
      for (i = 0; i < objArray.length; ++i)
      {
         k = addPrerequisites(objArray[i], orderMap, k);
      }

      // all objects should have been noted in orderMap (i.e. same content), else algorithm error
      assert orderMap.size() == objArray.length;

      Arrays.fill(objArray, null); // empty array for validation

      for (Lookup.Iterator/*<RelationalObject, Integer>*/ itr = orderMap.valueIterator();
           itr.hasNext();)
      {
         i = ((Integer)itr.next()).intValue();
         assert objArray[i] == null; // else algorithm error
         objArray[i] = (RelationalObject)itr.getKey();
      }

      return objArray;
   }

   /**
    * Should the specific index be created as a constraint.
    * @param index The index to check.
    * @return Should the specific index be created as a constraint.
    */
   protected boolean isConstraint(Index index)
   {
      return isPrimaryKey(index) && index.getTable().getType() == Table.MANAGED;
   }

   /**
    * Check if the specified table is allowed to have indexes.
    * @param table The table to check.
    * @return Can the specified table have indexes defined on it.
    */
   protected boolean isIndexable(Table table)
   {
      return table.getType() == Table.MANAGED;
   }

   /**
    * Check if the specified SQL Script is valid with respect to the specific RelationalSchema.
    * @param schema The schema to validate view tables against.
    * @param script The script to validate.
    * @return If the specified SQL Script is valid with respect to the specific RelationalSchema.
    */
   protected boolean isValidScript(SQLScript script, RelationalSchema schema)
   {
      try
      {
         getExecSQL(script, schema, null);
      }
      catch (MetadataException e)
      {
         return false;
      }

      return true;
   }

   /**
    * A function capable of parsing strings representing byte value with possible scale suffixes.
    * @param sValue The value to parse.
    * @return The value in bytes.
    * @throws NumberFormatException If the value is not parsable.
    */
   protected static long parseByteValue(String sValue) throws NumberFormatException
   {
      if (sValue == null)
      {
         return Long.parseLong(sValue); // no value
      }

      int nEnd = sValue.length();

      while (--nEnd >= 0 && !Character.isDigit(sValue.charAt(nEnd))); // find position of last digit

      long lValue = Long.parseLong(sValue.substring(0, ++nEnd).trim());
      String sScale = sValue.substring(nEnd).trim();

      if (sScale.length() == 0 || "B".equalsIgnoreCase(sScale)) // check bytes here as 'BB' invalid
      {
         return lValue; // value in bytes
      }

      if (sScale.length() == 1 ||
          (sScale.length() == 2 && "B".equalsIgnoreCase(sScale.substring(1))) || // EB/TB/GB/MB/KB
          (sScale.length() == 3 && // EiB/TiB/GiB/MiB/KiB SI case sensitive suffixes
           ("EiB".equals(sScale) || "PiB".equals(sScale) || "TiB".equals(sScale) ||
            "GiB".equals(sScale) || "MiB".equals(sScale) || "KiB".equals(sScale))))
      {
         long lScale = 1;

         switch (sScale.charAt(0)) // fall through from top to bottom
         {
            case 'E':
            case 'e':
               lScale <<= 10;
            case 'P':
            case 'p':
               lScale <<= 10;
            case 'T':
            case 't':
               lScale <<= 10;
            case 'G':
            case 'g':
               lScale <<= 10;
            case 'M':
            case 'm':
               lScale <<= 10;
            case 'K':
            case 'k':
               lScale <<= 10;
               return lValue * lScale; // don't bitshift lValue as it might be negative
         }
      }

      return Long.parseLong(sValue); // unsupported value, parse as byte value
   }

   /**
    * Sets the persistence adapter.
    * @param adapter The persistence adapter to set.
    */
   public void setAdapter(SQLAdapter adapter)
   {
      m_adapter = adapter;
   }

   /**
    * @return The persistence adapter.
    */
   public SQLAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * Sets the database fragment.
    * @param fragment The fragment to set. Can be null.
    */
   public void setFragment(RelationalDatabaseFragment fragment)
   {
      if (fragment == null)
      {
         setOwner(null);
      }
   }

   /**
    * Sets the SQL connection.
    * @param connection The SQL connection to set.
    */
   public void setConnection(Connection connection)
   {
      m_connection = connection;
   }

   /**
    * @return The SQL connection.
    */
   public Connection getConnection()
   {
      return m_connection;
   }

   /**
    * Sets the SQL appender.
    * @param appender The SQL appender to set.
    */
   public void setSQLAppender(SQLAppender appender)
   {
      m_appender = appender;
   }

   /**
    * @return The SQL appender.
    */
   public SQLAppender getSQLAppender()
   {
      return m_appender;
   }

   /**
    * Sets the metadata object owner.
    * @param sOwner The metadata object owner to set.
    */
   public void setOwner(String sOwner)
   {
      if (StringUtil.equalIgnoreCase(getDefaultOwner(), sOwner) || "".equals(sOwner))
      {
         sOwner = null;
      }

      m_sOwner = sOwner;
   }

   /**
    * @return The metadata object owner.
    */
   public String getOwner()
   {
      return (m_sOwner != null) ? m_sOwner : getDefaultOwner();
   }

   /**
    * @return The default schema owner name.
    */
   public String getDefaultOwner()
   {
      return null;
   }

   /**
    * @return The default pagesize to use for DB, or 0 for no default.
    */
   protected int getDefaultPageSize()
   {
      return 0;
   }

   /**
    * @return The default role granted schema access.
    */
   public String getDefaultRole()
   {
      return "njdbuser";
   }

   /**
    * @return The default tablespace name.
    */
   protected abstract String getDefaultTablespace();

   /**
    * @return The default index tablespace name.
    */
   protected abstract String getDefaultIndexspace();

   /**
    * @return The default long column tablespace name.
    */
   protected abstract String getDefaultLongspace();

   /**
    * Initializes the manager based on the schema.
    * @param schema The schema object.
    */
   protected void init(RelationalSchema schema)
   {
   }

   /**
    * Reads a relational schema from the database.
    * @param schema The destination schema.
    * @param sCatalogName The catalog name. Can be null.
    * @param sSchemaPattern The schema pattern with % and _ characters. Can be null.
    * @param sTablePattern The table pattern with % and _ characters. Can be null.
    * @param nameSet The table name set. Can be null to read all the tables.
    * @param progress The progress listener. Can be null.
    * @throws PersistenceException if an error occurs.
    */
   public void readSchema(RelationalSchema schema, String sCatalogName,
      String sSchemaPattern, String sTablePattern, Set nameSet, ProgressListener progress)
   {
      ResultSet rs = null;
      DatabaseMetaData dbmeta = null;
      boolean bPortable = true;
      int nTableCount = 0;
      Table lastTable;
      int nCurTable;

      assert schema.getTableCount() == 0;
      assert m_connection != null;

      schema.setPortable(false);

      try
      {
         Lookup tableMap = new HashTab();

         dbmeta = m_connection.getMetaData();

         // Read the tables

         if (progress != null)
         {
            progress.progress("info.sql.schemaManager.readingTables", null, 0);
         }

         sCatalogName = toDatabaseCase(sCatalogName);
         sSchemaPattern = toDatabaseCase(sSchemaPattern);
         sTablePattern = toDatabaseCase(sTablePattern);

         rs = dbmeta.getTables(sCatalogName, sSchemaPattern, sTablePattern, new String[]{"TABLE"});

         if (progress != null)
         {
            progress.progress("info.sql.schemaManager.readingTables", null, 0.01);
         }

         while (rs.next())
         {
            String sReadCatalogName = rs.getString("TABLE_CAT");
            String sSchemaName = toMetadataCase(rs.getString("TABLE_SCHEM"));
            String sTableName = rs.getString("TABLE_NAME");

            if (!isValidTableName(sTableName))
            {
               continue;
            }

            if (nameSet != null && !nameSet.contains(sTableName))
            {
               continue;
            }

            sTableName = toMetadataCase(sTableName);

            Table table = new Table(schema);

            table.setName(getFullTableName(sSchemaName, sTableName));
            table.setDescription(rs.getString("REMARKS"));
            table.setType(Table.EXTERNAL);
            ++nTableCount;

            if (s_logger.isDebugEnabled())
            {
               s_logger.debug("Read table \"" + ((sReadCatalogName == null) ? "" : sReadCatalogName) +
                  "." + ((sSchemaName == null) ? "" : sSchemaName) + "." + sTableName +
                  "\" -> \"" + table.getName() + "\"");
            }

            try
            {
               schema.addTable(table);
               tableMap.put(table, new String[]{sReadCatalogName, sSchemaName});
               bPortable &= isPortable(table);
            }
            catch (MetadataException e)
            {
               s_logger.error("Cannot add table \"" + table.getName() + "\"", e);
            }
         }

         rs.close();
         rs = null;

         if (nTableCount == 0)
         {
            nTableCount = 1;
         }

         // Read the columns

         rs = dbmeta.getColumns(sCatalogName, sSchemaPattern, sTablePattern, "%");
         lastTable = null;
         nCurTable = 0;

         Lookup2D caseInsensitiveSet = new HashTab2D(); // Object[Table][String]

         while (rs.next())
         {
            Table table = schema.findTable(
               getFullTableName(toMetadataCase(rs.getString("TABLE_SCHEM")),
                                toMetadataCase(rs.getString("TABLE_NAME"))));

            if (table == null)
            {
               continue;
            }

            if (progress != null && table != lastTable)
            {
               lastTable = table;
               caseInsensitiveSet.clear();
               progress.progress("info.sql.schemaManager.readingColumns", new Object[]{table.getName()},
                  0.05 + 0.15 * (nCurTable++ / nTableCount));
            }

            String sColName = toMetadataCase(rs.getString("COLUMN_NAME"));
            String sName = null;

            if (sColName != null)
            {
               sName = getCaseSensitiveName(sColName);

               if (isCaseInsensitive(sColName))
               {
                  caseInsensitiveSet.put(table, sName, Boolean.TRUE);
               }
            }

            if (isValidColumnName(sColName))
            {
               Column column = new Column(sName, table);

               column.setNullable(rs.getInt("NULLABLE") != DatabaseMetaData.columnNoNulls);
               column.setDescription(rs.getString("REMARKS"));

               String sType = rs.getString("TYPE_NAME");

               if (sType != null)
               {
                  sType = sType.toLowerCase(Locale.ENGLISH);
               }

               int nType = rs.getInt("DATA_TYPE");
               int nPrecision = rs.getInt("COLUMN_SIZE");
               int nScale = rs.getInt("DECIMAL_DIGITS");

               setType(column, sType, nType, nPrecision, nScale);

               Primitive type = column.getType();

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Read column \"" + table.getName() + '.' + sName + "\" " + sType +
                     '(' + nPrecision + ',' + nScale + "), SQLType=" + nType + " -> " +
                     ((type == null) ? "ignored: unsupported type" : type.getName() +
                     "(" + column.getPrecision() + "," + column.getScale() + "), allocation=" +
                     column.getAllocationString()));
               }

               if (type != null)
               {
                  try
                  {
                     table.addColumn(column);
                     bPortable &= isPortable(column);
                  }
                  catch (MetadataException e)
                  {
                     s_logger.error("Cannot add column \"" + sName + "\"", e);
                  }
               }
            }
         }

         rs.close();
         rs = null;

         // Set the case-sensitive columns

         for (Iterator tableItr = tableMap.iterator(); tableItr.hasNext();)
         {
            Table table = (Table)tableItr.next();

            for (int i = 0; i < table.getColumnCount(); ++i)
            {
               Column column = table.getColumn(i);

               if (!caseInsensitiveSet.contains(table, column.getName()))
               {
                  column.setCaseInsensitive(false);
               }
            }
         }

         // Read the indexes

         nCurTable = 0;

         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table table = (Table)itr.next();
            String[] names = (String[])tableMap.get(table);
            Index index = null;
            boolean bIgnore = false;

            if (progress != null)
            {
               progress.progress("info.sql.schemaManager.readingIndexes", new Object[]{table.getName()},
                  0.20 + 0.50 * (nCurTable++ / nTableCount));
            }

            rs = getIndexInfo(
                    names[0], toDatabaseCase(names[1]), toDatabaseCase(table.getTableName()));

            while (rs.next())
            {
               String sIndexName = toMetadataCase(rs.getString("INDEX_NAME"));

               if (sIndexName == null)
               {
                  continue;
               }

               String sColumnName = rs.getString("COLUMN_NAME");
               boolean bAscending = !"D".equals(rs.getString("ASC_OR_DESC"));
               boolean bUnique = !rs.getBoolean("NON_UNIQUE");
               int nType = rs.getInt("TYPE");

               sIndexName = generateIndexName(table.getTableName(), sIndexName, null);

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Read index column \"" + sIndexName + "." +
                     sColumnName + "\", ascending=" + bAscending);
               }

               if (index != null && !index.getName().equals(sIndexName))
               {
                  if (!bIgnore)
                  {
                     if (addIndex(table, index))
                     {
                        bPortable &= isPortable(index);
                     }
                  }

                  index = null;
                  bIgnore = false;
               }

               if (index == null)
               {
                  index = new Index(sIndexName, (nType == DatabaseMetaData.tableIndexClustered)
                                                ? Index.CLUSTER : Index.BTREE, table);
                  index.setUnique(bUnique);
               }

               if (!isValidColumnName(sColumnName))
               {
                  if (isCaseInsensitive(sColumnName))
                  {
                     sColumnName = getCaseSensitiveName(sColumnName);
                  }
                  else if (isFunctionalIndexSupported())
                  {
                     String sExpr = rs.getString("EXPR");

                     if (sExpr != null && sExpr.length() != 0)
                     {
                        String sName = getCaseSensitiveNameFromExpression(sExpr);

                        if (sName == null)
                        {
                           bIgnore = true;
                        }
                        else
                        {
                           sColumnName = sName;
                        }
                     }
                  }
                  else
                  {
                     bIgnore = true;
                  }
               }

               sColumnName = toMetadataCase(sColumnName);

               if (!bIgnore)
               {
                  try
                  {
                     index.addIndexColumn(new IndexColumn(table.getColumn(sColumnName), bAscending));
                  }
                  catch (MetadataException e)
                  {
                     s_logger.error("Cannot find column \"" + sColumnName +
                        "\", ignoring index \"" + sIndexName + "\"", e);
                     bIgnore = true;
                  }
               }
            }

            if (index != null && !bIgnore)
            {
               if (addIndex(table, index))
               {
                  bPortable &= isPortable(index);
               }
            }

            rs.close();
            rs = null;

            // Read the primary key

            rs = getPrimaryKeys(names[0], names[1], toDatabaseCase(table.getTableName()));
            index = null;
            bIgnore = false;

            try
            {
               while (rs.next())
               {
                  String sColumnName = toMetadataCase(rs.getString("COLUMN_NAME"));

                  if (!isValidColumnName(sColumnName))
                  {
                     if (!isCaseInsensitive(sColumnName))
                     {
                        continue;
                     }

                     sColumnName = getCaseSensitiveName(sColumnName);
                  }

                  if (index == null)
                  {
                     String sIndexName = rs.getString("PK_NAME");

                     if (sIndexName == null)
                     {
                        sIndexName = table.getName() + ".PK";
                        index = new Index(sIndexName, Index.BTREE, table);
                     }
                     else
                     {
                        sIndexName = generateIndexName(table.getTableName(), sIndexName, "PK");
                        index = table.findIndex(sIndexName);

                        if (index == null)
                        {
                           index = new Index(sIndexName, Index.BTREE, table);
                        }
                        else
                        {
                           table.setPrimaryKey(index);
                           bIgnore = true;

                           break;
                        }
                     }
                  }

                  if (s_logger.isDebugEnabled())
                  {
                     s_logger.debug("Read primary key column \"" + index.getName() + "." + sColumnName + "\"");
                  }

                  index.setUnique(true);
                  index.addIndexColumn(new IndexColumn(table.getColumn(sColumnName), true));
               }
            }
            catch (MetadataException e)
            {
               s_logger.error("Cannot add primary key to table \"" + table.getName() + "\"", e);
               bIgnore = true;
            }

            if (index != null)
            {
               if (!bIgnore)
               {
                  if (addIndex(table, index))
                  {
                     table.setPrimaryKey(index);
                     bPortable &= isPortable(index);
                  }
               }
            }
            else
            {
               for (int i = 0; i < table.getIndexCount(); ++i)
               {
                  index = table.getIndex(i);

                  if (index.isUnique())
                  {
                     table.setPrimaryKey(index);
                     
                     break;
                  }
               }
            }
            
            if (s_logger.isDebugEnabled())
            {
               if (table.getPrimaryKey() != null)
               {
                  s_logger.debug("The primary key of table \"" + table.getName() +
                     "\" is \"" + table.getPrimaryKey().getName() + "\"");
               }
               else
               {
                  s_logger.debug("Table \"" + table.getName() + "\" has no primary key");
               }
            }

            rs.close();
            rs = null;
         }

         // Read the foreign keys

         nCurTable = 0;

         for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
         {
            Table table = (Table)itr.next();
            String[] names = (String[])tableMap.get(table);
            Index index = null;
            boolean bIgnore = false;

            if (progress != null)
            {
               progress.progress("info.sql.schemaManager.readingForeignKeys", new Object[]{table.getName()},
                  0.70 + 0.30 * (nCurTable++ / nTableCount));
            }

            rs = dbmeta.getExportedKeys(names[0], names[1], table.getTableName());

            while (rs.next())
            {
               String sColumnName = toMetadataCase(rs.getString("FKCOLUMN_NAME"));
               String sSchemaName = rs.getString("FKTABLE_SCHEM");
               String sTableName = rs.getString("FKTABLE_NAME");
               String sIndexName = rs.getString("FK_NAME");

               Table foreignTable = schema.findTable(getFullTableName(sSchemaName, sTableName));

               if (foreignTable == null)
               {
                  continue;
               }

               sIndexName = generateIndexName(foreignTable.getTableName(), sIndexName, "FK" + (foreignTable.getIndexCount() + 1));

               if (index != null && !index.getName().equals(sIndexName))
               {
                  if (!bIgnore)
                  {
                     if (addIndex(index.getTable(), index))
                     {
                        addRelatedKey(table, index);
                        bPortable &= isPortable(index);
                     }
                  }

                  index = null;
                  bIgnore = false;
               }

               if (index == null)
               {
                  if (rs.getString("FK_NAME") == null)
                  {
                     index = new Index(sIndexName, Index.BTREE, foreignTable);
                  }
                  else
                  {
                     index = foreignTable.findIndex(sIndexName);

                     if (index == null)
                     {
                        index = new Index(sIndexName, Index.BTREE, foreignTable);
                     }
                     else
                     {
                        addRelatedKey(table, index);
                        bIgnore = true;
                     }
                  }
               }

               if (s_logger.isDebugEnabled())
               {
                  s_logger.debug("Read foreign key column \"" + index.getName() + "." + sColumnName + "\"");
               }

               if (!bIgnore)
               {
                  index.setType(Index.BTREE);
                  index.setUnique(false);

                  try
                  {
                     index.addIndexColumn(new IndexColumn(index.getTable().getColumn(sColumnName),
                        table.getPrimaryKey().getIndexColumn(index.getIndexColumnCount()).isAscending()));
                  }
                  catch (MetadataException e)
                  {
                     s_logger.error("Cannot add foreign key column to index \"" + index.getName() + "\"", e);
                     bIgnore = true;
                  }
               }
            }

            if (index != null && !bIgnore)
            {
                if (addIndex(index.getTable(), index))
                {
                   addRelatedKey(table, index);
                   bPortable &= isPortable(index);
                }
            }

            rs.close();
            rs = null;
         }

         schema.setPortable(bPortable);
      }
      catch (SQLException e)
      {
         throw new PersistenceException("err.persistence.schemaRead", e);
      }
      finally
      {
         if (rs != null)
         {
            try
            {
               rs.close();
            }
            catch (SQLException e)
            {
               s_logger.error("Error closing the SQL result set", e);
            }
         }
      }
   }

   /**
    * Sets column type attributes from JDBC type values.
    * @param column The column to modify. On return, the type is not set if unknown.  
    * @param sType Column JDBC type name, converted to lower case.
    * @param nType Column JDBC type code (one of java.sql.Types.* constants).
    * @param nPrecision Column JDBC precision (size for char and binary types).
    * @param nScale Column JDBC scale (decimal digits).
    */
   protected void setType(Column column, String sType, int nType, int nPrecision, int nScale)
   {
      byte nAllocation = Column.FIXED;
      Primitive type = null;

      switch (nType)
      {
         case Types.BIGINT:
            type = Primitive.LONG;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.BINARY:
            type = Primitive.BINARY;
            nScale = 0;
            break;

         case Types.BIT:
         case Types.BOOLEAN:
            type = Primitive.BOOLEAN;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.BLOB:
         case Types.LONGVARBINARY:
            type = Primitive.BINARY;

            if (nPrecision <= 0x4000)
            {
               nPrecision = Integer.MAX_VALUE;
            }

            nScale = 0;
            nAllocation = (nType == Types.BLOB) ? Column.LOCATOR : Column.VARYING;
            break;

         case Types.CHAR:
            if (sType.equals("uniqueidentifier"))
            {
               type = Primitive.BINARY;
               nPrecision = 16;
               nScale = 0;
            }
            else
            {
               type = Primitive.STRING;
               nScale = 0;
            }

            break;

         case Types.CLOB:
         case Types.LONGVARCHAR:
            type = Primitive.STRING;

            if (nPrecision <= 0x4000)
            {
               nPrecision = Integer.MAX_VALUE;
            }

            nScale = 0;
            nAllocation = (nType == Types.CLOB) ? Column.LOCATOR : Column.VARYING;
            break;

         case Types.DATE:
         case Types.TIME:
         case Types.TIMESTAMP:
            type = Primitive.TIMESTAMP;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.DECIMAL:
         case Types.NUMERIC:
            type = Primitive.DECIMAL;

            if (nScale == 0 && nPrecision <= 20)
            {
               if (nPrecision <= 10)
               {
                  type = Primitive.INTEGER;

                  if (nPrecision <= 3)
                  {
                     nPrecision = 1;
                  }
                  else if (nPrecision <= 5)
                  {
                     nPrecision = 2;
                  }
                  else
                  {
                     nPrecision = 0;
                  }
               }
               else
               {
                  type = Primitive.LONG;
                  nPrecision = 0;
               }
            }

            break;

         case Types.DOUBLE:
         case Types.FLOAT:
            type = Primitive.DOUBLE;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.INTEGER:
            type = Primitive.INTEGER;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.SMALLINT:
            type = Primitive.INTEGER;
            nPrecision = 2;
            nScale = 0;
            break;

         case Types.TINYINT:
            type = Primitive.INTEGER;
            nPrecision = 1;
            nScale = 0;
            break;

         case Types.REAL:
            type = Primitive.FLOAT;
            nPrecision = 0;
            nScale = 0;
            break;

         case Types.VARBINARY:
            type = Primitive.BINARY;
            nScale = 0;
            nAllocation = Column.VARYING;
            break;

         case Types.VARCHAR:
            type = Primitive.STRING;
            nScale = 0;
            nAllocation = Column.VARYING;
            break;

         default:
            if (sType.equals("nchar"))
            {
               type = Primitive.STRING;
               nPrecision >>= 1;
               nScale = 0;
            }
            else if (sType.equals("nvarchar2") || sType.equals("nvarchar"))
            {
               type = Primitive.STRING;
               nPrecision >>= 1;
               nScale = 0;
               nAllocation = Column.VARYING;
            }
            else if (sType.equals("binary_double"))
            {
               type = Primitive.DOUBLE;
               nPrecision = 0;
               nScale = 0;
            }
            else if (sType.equals("binary_float"))
            {
               type = Primitive.FLOAT;
               nPrecision = 0;
               nScale = 0;
            }
            else if (sType.startsWith("timestamp"))
            {
               type = Primitive.TIMESTAMP;
               nPrecision = 0;
               nScale = 0;
            }
            else if (sType.equals("nclob") || sType.equals("clob"))
            {
               type = Primitive.STRING;

               if (nPrecision <= 0x4000)
               {
                  nPrecision = Integer.MAX_VALUE;
               }

               nScale = 0;
               nAllocation = Column.LOCATOR;
            }
            else if (sType.equals("blob"))
            {
               type = Primitive.BINARY;

               if (nPrecision <= 0x4000)
               {
                  nPrecision = Integer.MAX_VALUE;
               }

               nScale = 0;
               nAllocation = Column.LOCATOR;
            }

            break;
      }

      if (nPrecision < 0)
      {
         nPrecision = 0;
      }

      if (type != null)
      {
         column.setType(type);
         column.setPrecision(nPrecision);
         column.setScale(nScale);
         column.setAllocation(nAllocation);
      }
   }

   /**
    * Determines if a table is portable.
    * @param table The table to check.
    * @return True if portable.
    */
   protected static boolean isPortable(Table table)
   {
      return table.getTableName().length() <= Table.MAX_NAME_LENGTH;
   }

   /**
    * Determines if the column is portable.
    * @param column The column to check.
    * @return True if portable.
    */
   protected static boolean isPortable(Column column)
   {
      return column.getName().length() <= Column.MAX_NAME_LENGTH;
   }

   /**
    * Determines if the index is portable.
    * @param index The index to check.
    * @return True if portable.
    */
   protected static boolean isPortable(Index index)
   {
      return index.getName().length() <= Index.MAX_NAME_LENGTH;
   }

   /**
    * Determines if a table should be read based on its name.
    * @param sName The table name. Can be null.
    * @return True if the table should be read.
    */
   protected boolean isValidTableName(String sName)
   {
      if (sName == null || sName.indexOf('$') >= 0)
      {
         return false;
      }

      return true;
   }

   /**
    * Determines if a column should be read based on its name.
    * @param sName The column name. Can be null.
    * @return True if the column should be read.
    */
   protected boolean isValidColumnName(String sName)
   {
      if (sName == null || sName.indexOf('$') >= 0)
      {
         return false;
      }

      return true;
   }

   /**
    * Determines if a column name corresponds to a case-insensitive column.
    * @param sName The raw column name in the database.
    * @return True if the column is case insensitive.
    */
   protected boolean isCaseInsensitive(String sName)
   {
      return false;
   }

   /**
    * Gets the case-sensitive column name from a raw name.
    * @param sName The raw column name in the database.
    * @return The case-sensitive column name.
    */
   protected String getCaseSensitiveName(String sName)
   {
      return sName;
   }

   /**
    * Gets a column name from a functional index expression.
    * @param sExpr The expression.
    * @return The column name, or null if nor found.
    */
   protected String getCaseSensitiveNameFromExpression(String sExpr)
   {
      return null;
   }

   /**
    * Normalizes a schema object name for use in the database.
    * @param sName The schema object name.
    * @return The normalized name.
    */
   protected String toDatabaseCase(String sName)
   {
      if (sName == null)
      {
         return sName;
      }

      return sName.toUpperCase(Locale.ENGLISH);
   }

   /**
    * Normalizes a schema object name case for use in the metadata.
    * @param sName The schema object name.
    * @return The normalized name.
    */
   protected String toMetadataCase(String sName)
   {
      if (sName == null)
      {
         return null;
      }

      if (sName.toUpperCase(Locale.ENGLISH).equals(sName))
      {
         return sName.toLowerCase(Locale.ENGLISH);
      }

      return sName;
   }

   /**
    * Creates a result set with index information.
    * @param sCatalog The catalog name.
    * @param sSchema The schema name.
    * @param sTable The table name.
    * @return The result set according to the JDBC spec.
    */
   protected ResultSet getIndexInfo(String sCatalog, String sSchema, String sTable) throws SQLException
   {
      return m_connection.getMetaData().getIndexInfo(sCatalog, sSchema, sTable, false, true);
   }

   /**
    * Return the index that is the actual primary key as defined in the RDBMS.
    * @param table The table to get the primary key for (not null).
    * @return The primary key index as seen by RDBMS.
    */
   protected Index getPrimaryKey(Table table)
   {
      Index pk = table.getPrimaryKey();

      return (pk != null && pk.getType() >= Index.BTREE) ? pk : null;
   }

   /**
    * Creates a result set with descriptions of the given table's primary key columns.
    * @param sCatalog The catalog name.
    * @param sSchema The schema name.
    * @param sTable The table name.
    * @return The result set according to the JDBC spec.
    * @see java.sql.DatabaseMetaData#getPrimaryKeys(java.lang.String, java.lang.String, java.lang.String)
    */
   protected ResultSet getPrimaryKeys(String sCatalog, String sSchema, String sTable) throws SQLException
   {
      return m_connection.getMetaData().getPrimaryKeys(sCatalog, sSchema, sTable);
   }

   /**
    * @return True if functional indexes are supported.
    */
   protected boolean isFunctionalIndexSupported()
   {
      return false;
   }

   /**
    * Return if the index is the actual primary key for its table as defined in the RDBMS.
    * @param index The index to check.
    * @return If the index is the actual primary key for its table as defined in the RDBMS.
    */
   protected boolean isPrimaryKey(Index index)
   {
      return index != null && index == getPrimaryKey(index.getTable());
   }

   /**
    * Generates an index name based on a table name.
    * @param sTableName The table name.
    * @param sIndexName The index name, can be null.
    * @param sDefault The default index name, if sIndexName is null.
    * @return The generated index name: table.index.
    */
   protected String generateIndexName(String sTableName, String sIndexName, String sDefault)
   {
      if (sIndexName != null)
      {
         if (sIndexName.startsWith(sTableName))
         {
            if (sIndexName.length() != sTableName.length())
            {
               sIndexName = sIndexName.substring(sTableName.length());

               while (sIndexName.charAt(0) == '_' && sIndexName.length() > 1)
               {
                  sIndexName = sIndexName.substring(1);
               }
            }
            else if (sDefault != null)
            {
               sIndexName = sDefault;
            }
         }
         else if (isIndexNameGlobal())
         {
            return sIndexName;
         }
      }
      else
      {
         sIndexName = sDefault;
      }

      return sTableName + '.' + sIndexName;
   }

   /**
    * @return True if the index names are global in the schema.
    */
   protected boolean isIndexNameGlobal()
   {
      return false;
   }

   /**
    * Adds an index to a given table.
    * @param table The table where to add the index.
    * @param index The index to add.
    * @return True if the operation succeeded.
    */
   protected static boolean addIndex(Table table, Index index)
   {
      try
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Read index \"" + index.getName() + "\", unique=" + index.isUnique() +
               ", type=" + index.getType());
         }

         table.addIndex(index);
      }
      catch (MetadataException e)
      {
         s_logger.error("Cannot add index \"" + index.getName() + "\" to table \"" + table.getName() + "\"", e);
         return false;
      }

      return true;
   }

   /**
    * Adds a foreign key referencing a given table.
    * @param table The referenced table containing the primary key.
    * @param index The foreign key.
    */
   protected static void addRelatedKey(Table table, Index index)
   {
      table.addRelatedKey(index);

      if (s_logger.isDebugEnabled())
      {
         s_logger.debug("Added foreign key \"" + index.getName() +
            "\" to table \"" + index.getTable().getName() +
            "\" referencing table \"" + table.getName()+ "\"");
      }
   }

   /**
    * Append an SQL script to the appender, one statement at a time delimited by getSeparator().
    * @param sSQL The script to append.
    */
   protected void appendSQLScript(String sScript)
   {
      String[] sqlArray = sScript.split(getSeparator().replace(SysUtil.LINE_SEP, "\r?\n"));

      for (int i = 0; i < sqlArray.length; ++i)
      {
         String sSQL = sqlArray[i].trim();

         if (sSQL.length() != 0)
         {
            m_appender.appendSQL(formatSQL(sSQL));
         }
      }
   }

   /**
    * Create a map of default properties for a given schema and overrides.
    * If multiple defaults available then first value will be returned.
    * @param schema The relational schema to create the database for.
    * @param config The custom values for template related properties.
    * @return Map of default properties for a schema taking into consideration supplied defaults.
    */
   public PropertyMap getDefaultDatabaseProperties(RelationalSchema schema, PropertyMap config)
   {
      String sScriptName = getSetupEtcScriptName();
      PropertyMap propertyMap = new TransferObject();
      PropertyMap referenceMap = setDefaultDatabaseProperties(new TransferObject());

      for (PropertyIterator itr = referenceMap.getIterator(); itr.hasNext();)
      {
         itr.next();
         itr.setValue(null); // unset only default values so that they do not override custom later
      }

      setCustomDatabaseProperties(referenceMap, config);
      setMetadataDatabaseProperties(referenceMap, schema);

      if (sScriptName != null)
      {
         InputStream istream = null;

         try
         {
            istream = URLUtil.openResource(getClass(), "etc/" + sScriptName);
            setTemplateDefaults(propertyMap,
                                referenceMap,
                                new InputStreamReader(istream, XMLUtil.ENCODING),
                                schema.getMetadata().isTestEnvironment());
         }
         catch (Throwable t)
         {
            ObjUtil.rethrow(t);
         }
         finally
         {
            IOUtil.close(istream);
         }
      }

      PropertyMap defaultsMap = setDefaultDatabaseProperties(new TransferObject());

      // look for unset keys needing to be overridden with custom values or defaults
      for (PropertyIterator itr = propertyMap.getIterator(); itr.hasNext();)
      {
         String sKey = (String)itr.next();
         Object value = referenceMap.findValue(sKey);

         if (value != null ||
             (itr.getValue() == null && (value = defaultsMap.findValue(sKey)) != null))
         {
            itr.setValue(value); // override default in template with custom value in schema
         }
      }

      return propertyMap;
   }

   /**
    * Set the key/value pairs for database template values which are taken from the user config.
    * @param map The map to modify (not null).
    * @param customMap The custom user values to use.
    * @return The PropertyMap that was used/modified.
    */
   public PropertyMap setCustomDatabaseProperties(PropertyMap map, PropertyMap customMap)
   {
      if (customMap == null)
      {
         map.setValue("collation", null);
         map.setValue("datapath", null);
         map.setValue("indexpath", null);
         map.setValue("indexspacesize", null);
         map.setValue("indexspaceincrement", null);
         map.setValue("longpath", null);
         map.setValue("longspacesize", null);
         map.setValue("longspaceincrement", null);
         map.setValue("mempercent", null);
         map.setValue("memsizeperm", null);
         map.setValue("memsizetemp", null);
         map.setValue("tablespacesize", null);
         map.setValue("tablespaceincrement", null);
         map.setValue("tempspace", null);
         map.setValue("tempspacesize", null);
         map.setValue("tempspaceincrement", null);
         map.setValue("undopath", null);
         map.setValue("undospace", null);
         map.setValue("undospacesize", null);
         map.setValue("undospaceincrement", null);

         return map;
      }

      Object dataPath = customMap.findValue("datapath");
      Object indexPath = customMap.findValue("indexpath");
      Object longPath = customMap.findValue("longpath");
      Object undoPath = customMap.findValue("undopath");

      map.setValue("collation", customMap.findValue("collation"));
      map.setValue("datapath", dataPath);
      map.setValue("indexpath", (indexPath == null) ? dataPath : indexPath);
      map.setValue("longpath", (longPath == null) ? dataPath : longPath);
      map.setValue("tempspace", customMap.findValue("tempspace"));
      map.setValue("undopath", (undoPath == null) ? dataPath : undoPath);
      map.setValue("undospace", customMap.findValue("undospace"));
      map.setValue("indexspacesize", formatByteSize((String)customMap.findValue("indexspacesize")));
      map.setValue(
         "indexspaceincrement", formatByteSize((String)customMap.findValue("indexspaceincrement")));
      map.setValue("longspacesize", formatByteSize((String)customMap.findValue("longspacesize")));
      map.setValue(
         "longspaceincrement", formatByteSize((String)customMap.findValue("longspaceincrement")));
      map.setValue("mempercent", formatPercent((String)customMap.findValue("mempercent")));
      map.setValue("memsizeperm", formatByteSize((String)customMap.findValue("memsizeperm")));
      map.setValue("memsizetemp", formatByteSize((String)customMap.findValue("memsizetemp")));
      map.setValue("tablespacesize", formatByteSize((String)customMap.findValue("tablespacesize")));
      map.setValue(
         "tablespaceincrement", formatByteSize((String)customMap.findValue("tablespaceincrement")));
      map.setValue("tempspacesize", formatByteSize((String)customMap.findValue("tempspacesize")));
      map.setValue(
         "tempspaceincrement", formatByteSize((String)customMap.findValue("tempspaceincrement")));
      map.setValue("undospacesize", formatByteSize((String)customMap.findValue("undospacesize")));
      map.setValue(
         "undospaceincrement", formatByteSize((String)customMap.findValue("undospaceincrement")));

      return map;
   }

   /**
    * Set the key/value pairs for database template values which have hardcoded defaults.
    * @param map The map to modify (not null).
    * @return The PropertyMap that was used/modified.
    */
   protected PropertyMap setDefaultDatabaseProperties(PropertyMap map)
   {
      assert map != null;

      map.setValue("indexspace", getDefaultIndexspace());
      map.setValue("longspace", getDefaultLongspace());
      map.setValue("pagesize",
                   (getDefaultPageSize() == 0) ? null : Integer.toString(getDefaultPageSize()));
      map.setValue("role", getDefaultRole());
      map.setValue("tablespace", getDefaultTablespace());
      map.setValue("user", getDefaultOwner());

      return map;
   }

   /**
    * Set the key/value pairs for database template values which are available from metadata.
    * @param map The map to modify (not null).
    * @param schema The schema to get values from.
    * @return The PropertyMap that was used/modified.
    */
   protected PropertyMap setMetadataDatabaseProperties(PropertyMap map, RelationalSchema schema)
   {
      if (schema == null)
      {
         map.setValue("adapter", null);
         map.setValue("database", null);
         map.setValue("host", null);
         map.setValue("indexfill", null);
         map.setValue("indexspace", null);
         map.setValue("instance", null);
         map.setValue("longspace", null);
         map.setValue("owner", null);
         map.setValue("pagesize", null);
         map.setValue("port", null);
         map.setValue("role", null);
         map.setValue("tablespace", null);
         map.setValue("user", null);
         map.setValue("password", null);

         return map;
      }

      RelationalDatabase rd = (RelationalDatabase)schema.getDataSource();
      RelationalDatabaseFragment rdf = (RelationalDatabaseFragment)rd.getDefaultFragment();
      String sIndexspace = ((RelationalDatabase)schema.getDataSource()).getIndexspaceName();
      String sLongspace = ((RelationalDatabase)schema.getDataSource()).getLongspaceName();
      String sTablespace = ((RelationalDatabase)schema.getDataSource()).getTablespaceName();

      sIndexspace = (sIndexspace == null) ? schema.getIndexspaceName() : sIndexspace;
      sLongspace = (sLongspace == null) ? schema.getLongspaceName() : sLongspace;
      sTablespace = (sTablespace == null) ? schema.getTablespaceName() : sTablespace;

      map.setValue("adapter", rd.getAdapter().getName());
      map.setValue("database", rdf.getDatabase());
      map.setValue("host", rdf.getHost());
      map.setValue("indexfill",
                   (schema.getIndexFill() == 0) ? null : Byte.valueOf(schema.getIndexFill()));
      map.setValue("indexspace", (DEFAULT_TABLESPACE.equals(sIndexspace)) ? null : sIndexspace);
      map.setValue("instance", rdf.getInstance());
      map.setValue("longspace", (DEFAULT_TABLESPACE.equals(sLongspace)) ? null : sLongspace);
      map.setValue("owner", getOwner());
      map.setValue("pagesize",
                   (rd.getPageSize() == 0) ? null : Primitive.createInteger(rd.getPageSize()));
      map.setValue("port", (rdf.getPort() == 0) ? null : Primitive.createInteger(rdf.getPort()));
      map.setValue("role", schema.getRoleName());
      map.setValue("tablespace", (DEFAULT_TABLESPACE.equals(sTablespace)) ? null : sTablespace);
      map.setValue("user", rdf.getUser());
      map.setValue("password", rdf.getPassword());

      return map;
   }

   /**
    * Collect default values from setup template.
    * Only the first value will be populated. A duplicate value would cause a warning.
    * If a value has no default in the template then it will be set to null.
    * @param propertyMap The map to add collected custom properties to (not null).
    * @param referenceMap Properties will be added only for the keys defined in this map (not null).
    * @param input The reader to query for input (not null).
    * @param bTest Include "iftest" sections from template.
    * @return The PropertyMap that was used/modified.
    * @throws IOException On error reading from input.
    */
   protected PropertyMap setTemplateDefaults(
      final PropertyMap propertyMap, final PropertyMap referenceMap,
      Reader input, final boolean bTest)
      throws IOException
   {
      // multiple pass over input, with each pass interpreting next depth level for unknown keys
      Reader reader = new SQLTemplateSubstReader(input)
       {
         protected Object getValue(String sKey, String sDefault)
         {
            if (referenceMap.hasValue(sKey)) // valid key
            {
               Object value = propertyMap.findValue(sKey);

               if (value == null) // valid key without value
               {
                  propertyMap.setValue(sKey, sDefault);
               }
               else if (sDefault != null && !value.equals(sDefault)) // new different default
               {
                  s_logger.warn("Multiple defaults detected for key: " + sKey);
               }
            }
            else if (bTest || !sKey.equals("iftest")) // invalid key, possibly requiring reparsing
            {
               try
               {
                  setTemplateDefaults(propertyMap, referenceMap, new StringReader(sDefault), bTest);
               }
               catch (IOException e)
               {
                  ObjUtil.rethrow(e); // wrap the exception to pass it out of this function
               }
            }

            return null;
         }
      };

      try
      {
         while (reader.read() >= 0); // consume entire input
      }
      catch (RuntimeException e)
      {
         if (e.getCause() instanceof IOException)
         {
            throw (IOException)e.getCause(); // unwrap and rethrow original IOException
         }

         throw e;
      }

      return propertyMap;
   }

   /**
    * Makes a full table name from the schema and the short table name.
    * @param sSchemaName The table schema name.
    * @param sTableName The short table name.
    */
   protected String getFullTableName(String sSchemaName, String sTableName)
   {
      if (m_sOwner != null)
      {
         sSchemaName = m_sOwner;
      }

      if (sSchemaName != null && sSchemaName.length() != 0 &&
         !StringUtil.equalIgnoreCase(sSchemaName, getDefaultOwner()))
      {
         return sSchemaName + "." + sTableName;
      }

      return sTableName;
   }

   /**
    * Appends to the SQL appender the SQL statements to create a Database.
    * @param schema The relational schema to create the database for (final for reader definition).
    * @param config The custom values for template related properties.
    */
   public void createDatabase(RelationalSchema schema, PropertyMap config)
   {
      String sScriptName = getSetupEtcScriptName();

      if (sScriptName == null)
      {
         return;
      }

      if (!(schema.getDataSource() instanceof RelationalDatabase) ||
          !(schema.getDataSource().getDefaultFragment() instanceof RelationalDatabaseFragment))
      {
         throw new IllegalStateException(); // this should not be user-triggerable
      }

      InputStream istream = null;
      StringWriter writer = new StringWriter();
      PropertyMap propertiesMap = new TransferObject();

      setCustomDatabaseProperties(propertiesMap, config);
      setMetadataDatabaseProperties(propertiesMap, schema);

      try
      {
         istream = URLUtil.openResource(getClass(), "etc/" + sScriptName);
         IOUtil.copy(writer,
                     new DatabaseTemplateSubstReader(
                        new InputStreamReader(istream, XMLUtil.ENCODING),
                        schema,
                        propertiesMap,
                        setDefaultDatabaseProperties(new TransferObject()),
                        (isWindowsCompatible(config)) ? '\\' : '/'));
      }
      catch (Throwable t)
      {
         ObjUtil.rethrow(t);
      }
      finally
      {
         IOUtil.close(istream);
      }

      appendSQLScript(writer.toString());
   }

   /**
    * Checks if any of the path parameters are in a Microsoft compatible format.
    * @param config Property map holding values for path parameters.
    * @return true if any of the path parameters are in Microsoft compatible format.
    */
   protected boolean isWindowsCompatible(PropertyMap config)
   {
      boolean bWindowsCompatible = false;
      String [] sPathArray = new String[]{"datapath", "indexpath", "longpath", "undopath"};

      for (int i = 0; i < sPathArray.length && !bWindowsCompatible; ++i)
      {
         String sPath = (String)config.findValue(sPathArray[i]);
         bWindowsCompatible = sPath != null &&
            (sPath.indexOf('\\') >= 0 ||
             sPath.length() >= 2 && sPath.charAt(1) == ':' && Character.isLetter(sPath.charAt(0)));
      }

      return bWindowsCompatible;
   }

   /**
    * Appends the SQL statements to create a schema to the SQL appender.
    * @param schema The relational schema.
    */
   public void createSchema(RelationalSchema schema)
   {
      script(getCreateEtcScriptName(), schema);

      RelationalObject[] orderedArray = getSortedRelationalObjects(schema);

      for (int i = 0; i < orderedArray.length; ++i)
      {
         RelationalObject obj = orderedArray[i];

         if (obj instanceof Table)
         {
            createTable((Table)obj);
         }
         else
         {
            createObject((SQLObject)obj);
         }
      }

      Table versionTable = schema.getVersionTable();

      // If the version table exists
      if (versionTable != null && versionTable.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(64);
         Metadata metadata = schema.getMetadata();
         SchemaVersion version = new SchemaVersion();

         version.setNamespace(metadata.getNamespace());
         version.setVersion(metadata.getVersion());
         version.setStep(UPGRADE_END_STEP);
         version.setUpgradable(false);
         version.setTest(false);
         appendVersionTableInsert(buf, schema, version, false); // set version to match schema
         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * Appends the SQL statements to create a given table to the SQL appender.
    * @param table The table object.
    */
   public void createTable(Table table)
   {
      init(table.getSchema());

      if (table.getType() == Table.MANAGED ||
          table.getType() == Table.QUERY ||
          table.getType() == Table.VIEW)
      {
         StringBuffer buf = new StringBuffer(128);

         if (table.getType() == Table.MANAGED)
         {
            // create the table

            buf.append("create table ");
            appendTableName(buf, table);
            buf.append('(');

            // add the table columns
            for (int i = 0; i < table.getColumnCount(); ++i)
            {
               Column column = table.getColumn(i);

               if (i > 0)
               {
                  buf.append(',');
               }

               if (i % 3 == 0)
               {
                  buf.append(SysUtil.LINE_SEP);
                  buf.append("   ");
               }
               else if (i > 0)
               {
                  buf.append(' ');
               }

               appendColumnDeclaration(buf, column, true, true, ", ");
            }

            if (isConstraint(getPrimaryKey(table)))
            {
               appendPrimaryKey(buf, table);
            }

            buf.append(SysUtil.LINE_SEP);
            buf.append(')');
            appendTableSuffix(buf, table);
         }
         else
         {
            appendViewDeclaration(buf, table);
         }

         m_appender.appendSQL(buf.toString());

         createIndexes(table); // create the table indexes
         grant(table);
      }
   }

   /**
    * Appends SQL using a table stored with "original" definition and recreating it according to
    * the "modified" definition. Data is preserved for columns with matching names.
    * @param target The modified table definition that the table must match after recreation.
    * @param current The original table definition currently persisted in RDBMS.
    * @param columnMap Map of new column name to old column name for column data preservation,
    *                  null map or missing keys == preserve column data,
    *                  null values == do not preserve column data.
    * Note: on error this implementation will leave original table renamed as table.getName() + "$"
    */
   protected void recreateTable(Table target, Table current, Lookup/*<String, String>*/ columnMap)
   {
      if (current.getType() != Table.MANAGED)
      {
         createTable(target);

         return; // nothing to do
      }

      Table original = current;

      if (target.getTableName().equals(current.getTableName()) &&
         ObjUtil.equal(target.getOwnerName(), current.getOwnerName())) // rename table before recreation
      {
         Table tmpTable = current.cloneTemporary(); // need different object for renameTable()

         for (int i = tmpTable.getIndexCount() - 1; i >= 0; --i) // backwards due to index removal
         {
            Index index = tmpTable.getIndex(i);

            if (target.findIndex(index.getName()) != null) // only discard index collisions
            {
               discardIndex(index, true); // discard index to avoid name clashes
            }
         }

         tmpTable.setName(getTableName(current, "$", false));
         renameTable(tmpTable, current); // move original table out of the way
         current = tmpTable;
      }

      createTable(target); // create table according to new definition
      copyTable(target, current, columnMap); // copy data with possible conversion
      dropTable(current, false); // drop original table
      original.copyFrom(target); // table definitions are identical after recreate
   }

   /**
    * Append list of privileges granted by default to an entity.
    * @param buf The buffer to append privileges to.
    * @param table The entity to grant privileges to.
    */
   protected void appendPrivileges(StringBuffer buf, Table table)
   {
      buf.append("select, insert, update, delete, references");
   }

   /**
    * Appends the SQL statements to grant access to a given table to the SQL appender.
    * @param table The table object.
    */
   protected void grant(Table table)
   {
      if (table.getType() == Table.MANAGED ||
          table.getType() == Table.QUERY ||
          table.getType() == Table.VIEW)
      {
         String sRole = getRole(table.getSchema());

         if (!StringUtil.isEmpty(sRole))
         {
            StringBuffer buf = new StringBuffer(128);

            buf.append("grant ");
            appendPrivileges(buf, table);
            buf.append(" on ");
            appendTableName(buf, table);
            buf.append(" to ");
            buf.append(quote(sRole));
            m_appender.appendSQL(buf.toString());
         }
      }
   }

   /**
    * Appends the SQL statements to rename a table to the SQL appender.
    * @param newTable The new table.
    * @param oldTable The old table.
    */
   protected void renameTable(Table newTable, Table oldTable)
   {
      createTable(newTable);
      copyTable(newTable, oldTable, null);
      dropTable(oldTable, false);
   }

   /**
    * Appends SQL for copying one table into another. The columns should match by name.
    * @param dstTable The destination table.
    * @param srcTable The source table.
    * @param columnMap Map of new column name to old column name for column data preservation,
    *                  null map or missing keys == preserve column data,
    *                  null values == do not preserve column data.
    */
   public void copyTable(Table dstTable, Table srcTable, Lookup/*<String, String>*/ columnMap)
   {
      if (srcTable.getType() != Table.MANAGED || dstTable.getType() != Table.MANAGED)
      {
         return; // nothing to do
      }

      StringBuffer insertBuf = new StringBuffer(256);
      StringBuffer selectBuf = new StringBuffer(256);
      boolean bExecute = false;

      insertBuf.append("insert into ");
      appendTableName(insertBuf, dstTable);
      insertBuf.append('(');
      selectBuf.append(") select ");

      for (int i = 0, nColCount = dstTable.getColumnCount(); i < nColCount; ++i)
      {
         Column dstCol = dstTable.getColumn(i);
         String sDstColName = dstCol.getName();
         String sSrcColName = (columnMap != null && columnMap.contains(sDstColName))
                            ? (String)columnMap.get(sDstColName) : sDstColName;
         Column srcCol = (sSrcColName == null) ? null : srcTable.findColumn(sSrcColName);

         if (srcCol != null) // column requiring data to be preserved
         {
            if (bExecute)
            {
               insertBuf.append(", ");
               selectBuf.append(", ");
            }

            appendColumn(insertBuf, dstCol, ", ");
            appendColumn(selectBuf, srcCol, dstCol, ", ");
            bExecute = true;
         }
      }

      insertBuf.append(selectBuf);
      insertBuf.append(" from ");
      appendTableName(insertBuf, srcTable);

      if (bExecute) // do not execute copy if there is nothing to copy
      {
         m_appender.appendSQL(insertBuf.toString());
      }
   }

   /**
    * Copies a column data into another column.
    * @param dstColumn The destination column.
    * @param srcColumn The source column.
    */
   protected void copyColumn(Column dstColumn, Column srcColumn)
   {
      Table table = dstColumn.getTable();

      assert ObjUtil.equal(table.getName(), srcColumn.getTable().getName());

      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(128);

         buf.append("update ");
         appendTableName(buf, table);

         buf.append(" set ");
         appendColumnAssignment(buf, dstColumn, srcColumn);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @return The keywords that start an add column definition.
    */
   protected String getAddColumnToken()
   {
      return " add ";
   }

   /**
    * Appends SQL for altering a column to the SQL appender.
    * @param newColumn The new column.
    * @param oldColumn The old column.
    */
   protected void alterColumn(Column newColumn, Column oldColumn)
   {
      Table table = newColumn.getTable();

      if (table.getType() == Table.MANAGED)
      {
         if (!isImplicitConversion(oldColumn, newColumn))
         {
            recreateTable(newColumn.getTable(), oldColumn.getTable(), null);

            return;
         }

         StringBuffer buf = new StringBuffer(128);

         buf.append("alter table ");
         appendTableName(buf, table);
         buf.append(getAlterColumnToken());
         appendColumnAlteration(buf, newColumn, oldColumn, null);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * Can the DB implicitly convert values from 'source' column to 'target' column.
    * @param source The current column definition in DB.
    * @param target The required DB column definition after alteration.
    * @return The DB can implicitly convert column values.
    */
   protected abstract boolean isImplicitConversion(Column source, Column target);

   /**
    * @return The keywords that start a drop column definition.
    */
   protected String getAlterColumnToken()
   {
      return " alter column ";
   }

   /**
    * Appends SQL for renaming a column to the SQL appender.
    * @param newColumn The new column.
    * @param oldColumn The old column.
    */
   protected void renameColumn(Column newColumn, Column oldColumn)
   {
      dropIndexes(oldColumn, false);
      createColumn(newColumn, false);
      copyColumn(newColumn, oldColumn);

      if (!newColumn.isNullable())
      {
         alterColumn(newColumn, oldColumn);
      }

      dropColumn(oldColumn);
      createIndexes(newColumn);
   }

   /**
    * Appends SQL for dropping a column to the SQL appender.
    * @param column The column to drop.
    */
   protected void dropColumn(Column column)
   {
      Table table = column.getTable();

      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("alter table ");
         appendTableName(buf, table);
         buf.append(getDropColumnToken());
         appendColumn(buf, column, getAlterColumnSeparator() + getDropColumnToken());

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @return The token that separates multiple column definitions in an alter statement.
    */
   protected String getAlterColumnSeparator()
   {
      return "";
   }

   /**
    * @return The keywords that start a drop column definition.
    */
   protected String getDropColumnToken()
   {
      return " drop column ";
   }

   /**
    * Appends a column assignment clause (dstColumn = srcColumn).
    * @param buf The destination string buffer.
    * @param dstColumn The destination column.
    * @param srcColumn The source column.
    */
   protected void appendColumnAssignment(StringBuffer buf, Column dstColumn, Column srcColumn)
   {
      appendColumnName(buf, dstColumn);
      buf.append(" = ");
      appendColumnName(buf, srcColumn);
   }

   /**
    * Appends a column name to a selection list in a string buffer.
    * @param buf The destination string buffer.
    * @param column The column to append.
    * @param sSep The separator for additional columns.
    */
   protected void appendColumn(StringBuffer buf, Column column, String sSep)
   {
      appendColumnName(buf, column);
   }

   /**
    * Appends a column name with its type and case converted for target column,
    * to a selection list in a string buffer.
    * @param buf The destination string buffer (not null).
    * @param column The column to append (not null).
    * @param target The target to convert to (not null).
    * @param sSep The separator for additional columns.
    */
   protected void appendColumn(StringBuffer buf, final Column column, Column target, String sSep)
   {
      if (isImplicitConversion(column, target))
      {
         appendColumnName(buf, column);
      }
      else // perform type conversion
      {
         m_adapter.appendTypeConversion(buf, null, column.getType(), target.getType(),
            new SQLGenerator(null, m_adapter)
         {
            protected void appendOperator(StringBuffer buf, Operator op)
            {
               appendColumnName(buf, column);
            }

            public String getOwner()
            {
               return SQLSchemaManager.this.getOwner();
            }
         });
      }
   }

   /**
    * Appends a single table column declaration to a string buffer.
    * @param buf The destination string buffer.
    * @param column The column to append.
    * @param sSuffix The column name suffix.
    * @param bNullability True to append the column nullability specification.
    * @param bCreateTable True if column declaration used for "create table".
    */
   protected void appendColumnDeclaration(StringBuffer buf, Column column, String sSuffix,
      boolean bNullability, boolean bCreateTable)
   {
      appendColumnName(buf, column, sSuffix);
      buf.append(' ');
      appendColumnType(buf, column);
      appendColumnSuffix(buf, column);

      if (bNullability)
      {
         if (!column.isNullable())
         {
            buf.append(" not");
         }

         buf.append(" null");
      }
   }

   /**
    * Appends a single column alteration to a string buffer.
    * @param buf The destination string buffer.
    * @param newColumn The altered column.
    * @param oldColumn The original column.
    * @param sSuffix The column name suffix.
    */
   protected void appendColumnAlteration(StringBuffer buf, Column newColumn, Column oldColumn, String sSuffix)
   {
      appendColumnDeclaration(buf, newColumn, sSuffix, true, false);
   }

   /**
    * Appends a table column declaration to a string buffer.
    * @param buf The destination string buffer.
    * @param column The column to use.
    * @param bNullability True to append the column nullability specification.
    * @param bCreateTable True if column declaration used for "create table".
    * @param sSep The separator for additional columns.
    */
   protected void appendColumnDeclaration(StringBuffer buf, Column column,
      boolean bNullability, boolean bCreateTable, String sSep)
   {
      appendColumnDeclaration(buf, column, null, bNullability, bCreateTable);
   }

   /**
    * Appends a table column type declaration to a string buffer.
    * @param buf The destination string buffer.
    * @param column The column to use.
    */
   protected abstract void appendColumnType(StringBuffer buf, Column column);

   /**
    * Appends a table column suffix (col type ... not null) to a string buffer.
    * @param buf The destination string buffer.
    * @param column The column to use.
    */
   protected void appendColumnSuffix(StringBuffer buf, Column column)
   {
   }

   /**
    * Appends the optional primary key constraint for a given table.
    * @param buf The destination string buffer.
    * @param table The table object.
    */
   protected void appendPrimaryKey(StringBuffer buf, Table table)
   {
      Index index = getPrimaryKey(table);

      if (index != null)
      {
         buf.append(',');
         buf.append(SysUtil.LINE_SEP);
         buf.append("   ");
         appendPrimaryKey(buf, index);
      }
   }

   /**
    * Appends a primary key constraint for a given table.
    * @param buf The destination string buffer.
    * @param index The primary key index.
    */
   protected void appendPrimaryKey(StringBuffer buf, Index index)
   {
      Table table = index.getTable();

      buf.append("constraint ");
      buf.append(getIndexName(index.getName(), table.getPrefix(), table.getOwnerName(), true, true));
      buf.append(" primary key");
      appendPrimaryKeyPrefix(buf, index);
      appendIndexColumns(buf, index);
      appendPrimaryKeySuffix(buf, index);
   }

   /**
    * Appends a primary key constraint prefix (constraint obj primary key ... ()) to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendPrimaryKeyPrefix(StringBuffer buf, Index index)
   {
   }

   /**
    * Appends a primary key constraint suffix (constraint () ...) to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendPrimaryKeySuffix(StringBuffer buf, Index index)
   {
   }

   /**
    * Appends the suffix for creating a given table (after create table() ...) to a string buffer.
    * @param buf The destination string buffer.
    * @param table The table object.
    */
   protected void appendTableSuffix(StringBuffer buf, Table table)
   {
   }

   /**
    * Appends an index creation SQL to the SQL appender.
    * @param index The index object.
    */
   protected void createIndex(Index index)
   {
      if (index.getType() >= Index.BTREE && isIndexable(index.getTable()))
      {
         if (!isConstraint(index))
         {
            createGenericIndex(index);
         }
         else if (isPrimaryKey(index))
         {
            createPrimaryKey(index);
         }
      }
   }

   /**
    * Appends a generic index creation SQL to the SQL appender.
    * @param index The index object.
    */
   protected void createGenericIndex(Index index)
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("create ");

      if (index.isUnique())
      {
         buf.append("unique ");
      }

      appendIndexPrefix(buf, index);

      buf.append("index ");
      buf.append(getIndexName(index, false, true));
      appendIndexDefinition(buf, index);
      appendIndexSuffix(buf, index);

      m_appender.appendSQL(buf.toString());
   }

   /**
    * Appends a primary key creation SQL to the SQL appender.
    * @param index The index object.
    */
   protected void createPrimaryKey(Index index)
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("alter table ");
      appendTableName(buf, index.getTable());
      buf.append(" add ");
      appendPrimaryKey(buf, index);

      m_appender.appendSQL(buf.toString());
   }

   /**
    * Appends SQL to create indexes containing a given column.
    * @param column The column.
    */
   protected void createIndexes(Column column)
   {
      Table table = column.getTable();

      for (int i = 0, n = table.getIndexCount(); i < n; ++i)
      {
         Index index = table.getIndex(i);

         if (index.findIndexColumn(column) != null)
         {
            createIndex(index);
         }
      }
   }

   /**
    * Create all non-inlined indexes for a given table.
    * @param table The table to create all non-inlined indexes for.
    */
   protected void createIndexes(Table table)
   {
      for (int i = 0, nCount = table.getIndexCount(); i < nCount; ++i)
      {
         Index index = table.getIndex(i);

         if (!isConstraint(index))
         {
            createIndex(index);
         }
      }
   }

   /**
    * Appends SQL for renaming an index to the SQL appender.
    * @param newIndex The new index.
    * @param oldIndex The old index.
    */
   protected void renameIndex(Index newIndex, Index oldIndex)
   {
      dropIndex(oldIndex);
      createIndex(newIndex);
   }

   /**
    * Gets an index name.
    * @param index The index object.
    * @param bConstraint True if the name is for a constraint clause.
    * @param bQuote True to quote the keywords.
    * @return The full index name.
    */
   protected String getIndexName(Index index, boolean bConstraint, boolean bQuote)
   {
      return getIndexName(index.getName(), index.getTable().getPrefix(),
               index.getTable().getOwnerName(), bConstraint, bQuote);
   }

   /**
    * Gets a fully qualified index name.
    * @param index The index object.
    * @param bQuote True to quote the keywords.
    */
   protected String getFullIndexName(Index index, boolean bQuote)
   {
      return getIndexName(index, isPrimaryKey(index), bQuote);
   }

   /**
    * Gets an index name.
    * @param sName The index name.
    * @param sPrefix The index name prefix.
    * @param sOwner The table owner, null to use the default one.
    * @param bConstraint True if the name is for a constraint clause.
    * @param bQuote True to quote the keywords.
    * @return The full index name.
    * @see nexj.core.persistence.sql.SQLAdapter#indexNameMatches(nexj.core.meta.persistence.sql.Index, java.lang.String)
    */
   protected String getIndexName(String sName, String sPrefix, String sOwner, boolean bConstraint, boolean bQuote)
   {
      StringBuffer buf = new StringBuffer(32);

      if (sPrefix != null)
      {
         buf.append(sPrefix);
      }

      for (int i = 0; i < sName.length(); ++i)
      {
         char ch = sName.charAt(i);

         if (ch == '.')
         {
            ch = '_';
         }

         buf.append(ch);
      }

      sName = buf.toString();

      if (bQuote)
      {
         sName = quote(sName);
      }

      if (!bConstraint)
      {
         if (sOwner == null)
         {
            sOwner = getOwner();
         }

         if (sOwner != null && sOwner.length() != 0)
         {
            if (bQuote)
            {
               sOwner = quote(sOwner);
            }

            sName = sOwner + '.' + sName;
         }
      }

      return sName;
   }

   /**
    * Appends the prefix for creating an index (create ... index) to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendIndexPrefix(StringBuffer buf, Index index)
   {
   }

   /**
    * Appends index columns to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendIndexColumns(StringBuffer buf, Index index)
   {
      buf.append('(');

      for (int i = 0; i < index.getIndexColumnCount(); ++i)
      {
         IndexColumn icol = index.getIndexColumn(i);

         if (i > 0)
         {
            buf.append(", ");
         }

         appendIndexColumn(buf, icol);
      }

      buf.append(')');
   }

   /**
    * Appends an index column to a string buffer.
    * @param buf The destination string buffer.
    * @param indexColumn The index column object.
    */
   protected void appendIndexColumn(StringBuffer buf, IndexColumn indexColumn)
   {
      appendIndexColumn(buf, indexColumn.getColumn());

      if (!indexColumn.isAscending())
      {
         buf.append(" desc");
      }
   }

   /**
    * Appends an index column name to a string buffer.
    * @param buf The destination string buffer.
    * @param column The column object.
    */
   protected void appendIndexColumn(StringBuffer buf, Column column)
   {
      appendColumnName(buf, column);
   }

   /**
    * Appends the suffix for creating an index (create index on obj() ...) to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendIndexSuffix(StringBuffer buf, Index index)
   {
   }

   /**
    * Appends an index drop SQL to the SQL appender.
    * @param index The index object.
    */
   protected void dropIndex(Index index)
   {
      if (index.getType() >= Index.BTREE && isIndexable(index.getTable()))
      {
         if (isConstraint(index))
         {
            dropConstraint(index);
         }
         else
         {
            dropGenericIndex(index);
         }
      }
   }

   /**
    * Appends a generic index drop SQL to the SQL appender.
    * @param index The index object.
    */
   protected void dropGenericIndex(Index index)
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("drop index ");
      buf.append(getFullIndexName(index, true));
      appendDropIndexSuffix(buf, index);
      m_appender.appendSQL(buf.toString());
   }

   /**
    * Appends a constraint drop SQL to the SQL appender.
    * @param index The index object.
    */
   protected void dropConstraint(Index index)
   {
      StringBuffer buf = new StringBuffer(128);

      buf.append("alter table ");
      appendTableName(buf, index.getTable());
      buf.append(" drop constraint ");
      buf.append(getIndexName(index, true, true));
      m_appender.appendSQL(buf.toString());
   }

   public final void dropSchema(RelationalSchema schema)
   {
      dropSchema(schema, false);
   }

   /**
    * Appends the SQL statements to drop a schema to the SQL appender.
    * @param schema The relational schema.
    * @param bOld true to drop old schema objects.
    */
   public void dropSchema(RelationalSchema schema, boolean bOld)
   {
      Metadata metadata = schema.getMetadata();
      RelationalObject[] orderedArray = getSortedRelationalObjects(schema);

      // drop all objects in reverse order
      for (int i = orderedArray.length - 1; i >= 0; --i)
      {
         RelationalObject obj = orderedArray[i];

         if (obj instanceof Table)
         {
            dropTable((Table)obj, true);
         }
         else
         {
            dropObject((SQLObject)obj);
         }
      }

      // test environments should also drop any tables mentioned in Main.upgrade
      if (bOld)
      {
         Upgrade upgrade = metadata.getUpgrade("Main");
         VersionUpgrade version = upgrade.getFirstVersion();
         Lookup/*<Object, UpgradeState>*/ stateMap = Upgrade.getInitialState(version);
         Lookup/*<String, List<Table>>*/ tableMap =
            new HashTab/*<String, List<Table>>*/(schema.getTableCount());

         addDropTable(tableMap, schema); // fill with full schema first before adding from upgrades
         version = findFirstVersion(version, stateMap, (RelationalDatabase)schema.getDataSource());

         for (VersionUpgrade u = version; u != null; u = u.getNext())
         {
            if (u instanceof RelationalSchemaUpgrade)
            {
               RelationalSchemaUpgrade rsu = (RelationalSchemaUpgrade)u;

               if (rsu.getDataSource().getSchema() == schema)
               {
                  RelationalSchemaUpgradeState state =
                     (RelationalSchemaUpgradeState)Upgrade.getState(stateMap, u);

                  for (int i = 0, nCount = rsu.getStepCount(); i < nCount; ++i)
                  {
                     rsu.getStep(i).apply(state); // apply every every step off this upgrade
                  }

                  addDropTable(tableMap, state.getSchema()); // add any differing definitions
               }
            }
         }

         List/*<Table>*/ tableList = new ArrayList/*<Table>*/();

         for (Iterator/*<List<Table>>*/ itr = tableMap.valueIterator(); itr.hasNext();)
         {
            tableList.addAll((Collection)itr.next());
         }

         Collections.sort(tableList, TABLE_COMPARATOR); // sort table array in predictable order

         for (int i = tableList.size() - 1; i >= 0; --i)
         {
            Table table = (Table)tableList.get(i);
            Table dropSchemaTable = table.getSchema().findTable(table.getName());

            // ignore tables from main schema since they were dropped above
            if (table.getSchema() != schema)
            {
               // remove the table from its declaring schema for dropTable() dependency resolution
               // to work might be null for Main.upgrade tables dropped in some upgrade step
               if (dropSchemaTable != null)
               {
                  dropSchemaTable.getSchema().removeTable(dropSchemaTable);
               }

               dropTable(table, true);
            }
         }
      }

      script(getDropEtcScriptName(), schema); // drop last since might contain stored proc for drop
   }

   /**
    * Appends a drop SQL statement for a given table to the SQL appender.
    * @param table The table object.
    * @param bAll Whether dropping all tables.
    */
   public void dropTable(Table table, boolean bAll)
   {
      if (table.getType() == Table.MANAGED ||
          table.getType() == Table.QUERY ||
          table.getType() == Table.VIEW)
      {
         StringBuffer buf = new StringBuffer(64);

         if (table.getType() == Table.MANAGED)
         {
            buf.append("drop table ");
         }
         else
         {
            buf.append("drop view ");
         }

         appendTableName(buf, table);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * Appends the SQL statements to truncate all the tables in a schema (except the version table)
    * to the SQL appender.
    * @param schema The relational schema.
    */
   public void truncateSchema(RelationalSchema schema)
   {
      RelationalObject[] orderedArray = getSortedRelationalObjects(schema);
      Table versionTable = schema.getVersionTable();

      // drop all SQL Objects in reverse order, recreate them after
      for (int i = orderedArray.length - 1; i >= 0; --i)
      {
         RelationalObject obj = orderedArray[i];

         if (obj instanceof SQLObject)
         {
            dropObject((SQLObject)obj);
         }
         else
         {
            Table table = (Table)obj;

            if (table.getType() == Table.QUERY)
            {
               dropTable(table, false);
            }
         }
      }

      for (int i = 0; i < orderedArray.length; ++i)
      {
         RelationalObject obj = orderedArray[i];

         if (obj instanceof SQLObject)
         {
            createObject((SQLObject)obj);
         }
         else
         {
            Table table = (Table)obj;

            if (table.getType() == Table.QUERY)
            {
               createTable(table);
            }
            else if (table != versionTable)
            {
               truncateTable(table);
            }
         }
      }
   }

   /**
    * Appends a truncate SQL statement for a given table to the SQL appender.
    * @param table The table object.
    */
   public void truncateTable(Table table)
   {
      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("truncate table ");
         appendTableName(buf, table);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * Appends the SQL statements to gather statistics for all the the tables in a schema to the SQL appender.
    * @param schema The relational schema.
    */
   public void analyzeSchema(RelationalSchema schema)
   {
      Table[] tableArray = getSortedTables(schema);

      for (int i = 0; i < tableArray.length; ++i)
      {
         analyzeTable(tableArray[i]);
      }
   }

   /**
    * Appends the SQL statements to gather statistics for a given table to the SQL appender.
    * @param table The table object.
    */
   public abstract void analyzeTable(Table table);

   /**
    * Appends the SQL statements for a single step upgrade of a schema to the SQL appender.
    * @param upgrade The upgrade step container.
    * @param state The upgrade state, containing the modifiable schema.
    * @param version The current schema version. The step ordinal number is updated by this method.
    */
   public void upgrade(RelationalSchemaUpgrade upgrade,
      RelationalSchemaUpgradeState state, SchemaVersion version)
   {
      init(state.getSchema());

      m_upgrade = upgrade;
      m_state = state;
      m_nStep = version.getStep();

      if (m_nStep < 0)
      {
         throw new IllegalArgumentException("Version \"" + version.getVersion() + "\" upgrade already completed");
      }

      if (m_nStep < upgrade.getStepCount())
      {
         RelationalSchemaUpgradeStep step = upgrade.getStep(m_nStep++);

         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Upgrade version \"" + version.getVersion() +
               "\", step " + (m_nStep - 1) + ": " + step);
         }

         if (step instanceof CreateTableStep)
         {
            createTable((CreateTableStep)step);
         }
         else if (step instanceof AlterTableStep)
         {
            alterTable((AlterTableStep)step);
         }
         else if (step instanceof RenameTableStep)
         {
            renameTable((RenameTableStep)step);
         }
         else if (step instanceof DropTableStep)
         {
            dropTable((DropTableStep)step);
         }
         else if (step instanceof ApplyTableAspectStep)
         {
            applyTableAspect((ApplyTableAspectStep)step);
         }
         else if (step instanceof RemoveTableAspectStep)
         {
            removeTableAspect((RemoveTableAspectStep)step);
         }
         else if (step instanceof CreateColumnStep)
         {
            createColumn((CreateColumnStep)step);
         }
         else if (step instanceof AlterColumnStep)
         {
            alterColumn((AlterColumnStep)step);
         }
         else if (step instanceof RenameColumnStep)
         {
            renameColumn((RenameColumnStep)step);
         }
         else if (step instanceof DropColumnStep)
         {
            dropColumn((DropColumnStep)step);
         }
         else if (step instanceof CreateIndexStep)
         {
            createIndex((CreateIndexStep)step);
         }
         else if (step instanceof RenameIndexStep)
         {
            renameIndex((RenameIndexStep)step);
         }
         else if (step instanceof DropIndexStep)
         {
            dropIndex((DropIndexStep)step);
         }
         else if (step instanceof ApplyIndexAspectStep)
         {
            applyIndexAspect((ApplyIndexAspectStep)step);
         }
         else if (step instanceof RemoveIndexAspectStep)
         {
            removeIndexAspect((RemoveIndexAspectStep)step);
         }
         else if (step instanceof CreateObjectStep)
         {
            createObject((CreateObjectStep)step);
         }
         else if (step instanceof DropObjectStep)
         {
            dropObject((DropObjectStep)step);
         }
         else if (step instanceof ExecStep)
         {
            execSQL((ExecStep)step);
         }
         else if (step instanceof SupportAdapterStep)
         {
            // NOOP
         }
         else
         {
            throw new IllegalStateException();
         }
      }

      if (m_nStep >= upgrade.getStepCount())
      {
         if (s_logger.isDebugEnabled())
         {
            s_logger.debug("Completed upgrade version \"" + version.getVersion() + "\"");
         }

         m_nStep = UPGRADE_END_STEP;
      }

      version.setStep(m_nStep);
   }

   /**
    * Appends SQL for creating a table to the SQL appender and upgrades the metadata.
    * @param step The create table step.
    */
   protected void createTable(CreateTableStep step)
   {
      step.apply(m_state);
      createTable(step.getTable());
   }

   /**
    * Appends SQL for altering a table to the SQL appender and upgrades the metadata.
    * @param step The alter table step.
    */
   protected void alterTable(AlterTableStep step)
   {
      Table table = m_state.getSchema().getTable(step.getName());
      Table current = null;
      Index oldPK = getPrimaryKey(table);
      String sNewPK = step.getPrimaryKeyName();
      boolean bCreateIndex = sNewPK != null && (oldPK == null || !oldPK.getName().equals(sNewPK));

      if (bCreateIndex)
      {
         String sName = sNewPK;

         current = table.cloneTemporary();

         // check previous steps if PK was requested, if so then it would not have been created
         for (int i = m_nStep - 2; i >= 0; --i)
         {
            RelationalSchemaUpgradeStep st = m_upgrade.getStep(i);

            if (st instanceof CreateIndexStep)
            {
               if (((CreateIndexStep)st).getOutline().getName().equals(sName))
               {
                  // index was never created, due to an optimization
                  current.removeIndex(current.getIndex(sNewPK));
                  break;
               }
            }
            else if (st instanceof RenameIndexStep)
            {
               if (((RenameIndexStep)st).getNewName().equals(sName))
               {
                  sName = ((RenameIndexStep)st).getOldName();
               }
            }
            else if (!(st instanceof CreateColumnStep) &&
               !(st instanceof AlterColumnStep) &&
               !(st instanceof RenameColumnStep) &&
               !(st instanceof DropColumnStep) &&
               !(st instanceof DropIndexStep))
            {
               break;
            }
         }
      }

      step.apply(m_state);

      if (bCreateIndex)
      {
         Index newPK = getPrimaryKey(table);
         Table intermedary = current.cloneTemporary();

         // change/removal of PK requires drop of PK constraint/index
         if (oldPK != null && oldPK != newPK)
         {
            intermedary.removeIndex(intermedary.getIndex(oldPK.getName()));
         }

         if (newPK != null)
         {
            newPK = intermedary.findIndex(newPK.getName()); // find index to be promoted to PK
         }

         // an existing index exists that needs to be promoted to PK according to metadata
         if (newPK != null && newPK.getName().equals(sNewPK)) // PK may differ for e.g. Oracle
         {
            intermedary.removeIndex(newPK);
         }

         dropIndexes(intermedary, current);
         createIndexes(table, intermedary);
      }
   }

   /**
    * Appends SQL for renaming a table to the SQL appender and upgrades the metadata.
    * @param step The rename table step.
    */
   protected void renameTable(RenameTableStep step)
   {
      Table oldTable = m_state.getSchema().getTable(step.getOldName()).cloneTemporary();

      step.apply(m_state);
      renameTable(step.getTable(), oldTable);
   }

   /**
    * Appends SQL for dropping a table to the SQL appender and upgrades the metadata.
    * @param step The drop table step.
    */
   protected void dropTable(DropTableStep step)
   {
      step.apply(m_state);
      dropTable(step.getTable(), false);
   }

   /**
    * Appends SQL for applying a table aspect to the SQL appender and upgrades the metadata.
    * @param step The apply table aspect step.
    */
   protected void applyTableAspect(ApplyTableAspectStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      List/*<Table>*/ tableList = new ArrayList/*<Table>*/();

      for (Iterator itr = step.getPointcutIterator(schema.getTableIterator());
           itr.hasNext();
           tableList.add(((Table)itr.next()).cloneTemporary()));

      step.apply(m_state);

      SQLScriptHolder holder = step.getScriptHolder();

      for (int i = 0, nCount = tableList.size(); i < nCount; ++i)
      {
         Table current = (Table)tableList.get(i);
         Table target = schema.getTable(current.getName());

         createColumns(target, current, false); // create nullable columns
         execSQL(holder, current); // execute any defined scripts
         alterColumns(target, current); // set null constraint
         createIndexes(target, current); // create missing indexes
      }
   }

   /**
    * Appends SQL for removing a table aspect to the SQL appender and upgrades the metadata.
    * @param step The remove table aspect step.
    */
   protected void removeTableAspect(RemoveTableAspectStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      List/*<Table>*/ tableList = new ArrayList/*<Table>*/();

      for (Iterator itr = step.getPointcutIterator(schema.getTableIterator());
           itr.hasNext();
           tableList.add(((Table)itr.next()).cloneTemporary()));

      step.apply(m_state);

      for (int i = 0, nCount = tableList.size(); i < nCount; ++i)
      {
         Table current = (Table)tableList.get(i);
         Table target = schema.getTable(current.getName());

         dropIndexes(target, current); // drop extra indexes
         dropColumns(target, current); // drop extra columns
         createIndexes(target, current); // recreate any indexes that had the dropped column
      }
   }

   /**
    * Appends SQL for creating a column to the SQL appender and upgrades the metadata.
    * @param step The create column step.
    */
   protected void createColumn(CreateColumnStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Table table = schema.getTable(step.getTableName()); // table in the column is fully updated
      SQLScriptHolder holder = step.getScriptHolder();

      if (table.isAspect())
      {
         int nEnd = 0;
         Table[] tableArray = getSortedTables(schema);

         for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
         {
            if (tableArray[i].hasAspect(table))
            {
               tableArray[nEnd++] = tableArray[i].cloneTemporary();
            }
         }

         step.apply(m_state);

         for (int i = 0; i < nEnd; ++i)
         {
            Table current = tableArray[i];
            Table target = schema.getTable(current.getName());

            createColumns(target, current, false); // create nullable columns
            execSQL(holder, current); // execute any defined scripts
            alterColumns(target, current); // set null constraint
         }
      }
      else
      {
         Table current = table.cloneTemporary();

         step.apply(m_state);
         table = schema.getTable(step.getTableName()); // refresh table from schema, required!
         createColumns(table, current, false); // create nullable columns
         execSQL(holder, current); // execute any defined scripts
         alterColumns(table, current);
      }
   }

   /**
    * Appends SQL for altering a column to the SQL appender and upgrades the metadata.
    * @param step The alter column step.
    */
   protected void alterColumn(AlterColumnStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Table table = schema.getTable(step.getTableName());

      if (table.isAspect())
      {
         int nEnd = 0;
         Table[] tableArray = getSortedTables(schema);

         for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
         {
            if (tableArray[i].hasAspect(table))
            {
               tableArray[nEnd++] = tableArray[i].cloneTemporary();
            }
         }

         step.apply(m_state);

         for (int i = 0; i < nEnd; ++i)
         {
            Table current = tableArray[i];

            alterColumns(schema.getTable(current.getName()), current);
         }
      }
      else
      {
         Table current = table.cloneTemporary();

         step.apply(m_state);
         alterColumns(table, current);
      }
   }

   /**
    * Appends SQL for renaming a column to the SQL appender and upgrades the metadata.
    * @param step The rename column step.
    */
   protected void renameColumn(RenameColumnStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Table table = schema.getTable(step.getTableName());

      if (table.isAspect())
      {
         List oldColumnList = new ArrayList();
         Table[] tableArray = getSortedTables(schema);

         for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
         {
            Table pointcut = tableArray[i];

            if (pointcut.hasAspect(table))
            {
               oldColumnList.add(pointcut.cloneTemporary().getColumn(step.getOldName()));
            }
         }

         step.apply(m_state);

         for (int i = 0, n = oldColumnList.size(); i < n; ++i)
         {
            Column oldColumn = (Column)oldColumnList.get(i);

            renameColumn(schema.getTable(oldColumn.getTable().getName()).getColumn(step.getNewName()), oldColumn);
         }
      }
      else
      {
         Column oldColumn = table.cloneTemporary().getColumn(step.getOldName());

         step.apply(m_state);
         renameColumn(step.getColumn(), oldColumn);
      }
   }

   /**
    * Appends SQL for dropping a column to the SQL appender and upgrades the metadata.
    * @param step The drop column step.
    */
   protected void dropColumn(DropColumnStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Table table = schema.getTable(step.getTableName());

      if (table.isAspect())
      {
         int nEnd = 0;
         Table[] tableArray = getSortedTables(schema);

         for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
         {
            if (tableArray[i].hasAspect(table))
            {
               tableArray[nEnd++] = tableArray[i].cloneTemporary();
            }
         }

         step.apply(m_state);

         for (int i = 0; i < nEnd; ++i)
         {
            Table current = tableArray[i];

            dropColumns(schema.getTable(current.getName()), current);
         }
      }
      else
      {
         Table current = table.cloneTemporary();

         step.apply(m_state);
         dropColumns(table, current);
      }
   }

   /**
    * Appends SQL for creating an index to the SQL appender and upgrades the metadata.
    * @param step The create index step.
    */
   protected void createIndex(CreateIndexStep step)
   {
      step.apply(m_state);

      Index index = step.getIndex();

      if (!index.isAspect())
      {
         Table table = index.getTable();

         if (table.isAspect())
         {
            Table[] tableArray = getSortedTables(m_state.getSchema());

            for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
            {
               Table pointcut = tableArray[i];

               if (pointcut.hasAspect(table))
               {
                  Index pointcutIndex = pointcut.getIndex(index.getName(pointcut));

                  if (!isUpgradedToPrimaryKey(pointcutIndex.getName()))
                  {
                     createIndex(pointcutIndex);
                  }
               }
            }
         }
         else if (isIndexable(table) && !isUpgradedToPrimaryKey(index.getName()))
         {
            createIndex(index);
         }
      }
   }

   /**
    * Determines if an index is upgraded to a primary key in nearby steps.
    * @param sIndexName The index name.
    * @return True if the index is upgraded to a primary key.
    */
   protected boolean isUpgradedToPrimaryKey(String sIndexName)
   {
      for (int i = m_nStep, n = m_upgrade.getStepCount(); i < n; ++i)
      {
         RelationalSchemaUpgradeStep st = m_upgrade.getStep(i);

         if (st instanceof DropIndexStep)
         {
            if (((DropIndexStep)st).getName().equals(sIndexName))
            {
               break;
            }
         }
         else if (st instanceof RenameIndexStep)
         {
            if (((RenameIndexStep)st).getOldName().equals(sIndexName))
            {
               sIndexName = ((RenameIndexStep)st).getNewName();
            }
         }
         else if (st instanceof AlterTableStep)
         {
            if (sIndexName.equals(((AlterTableStep)st).getPrimaryKeyName()))
            {
               return true;
            }
         }
         else if (!(st instanceof CreateColumnStep) &&
            !(st instanceof AlterColumnStep) &&
            !(st instanceof RenameColumnStep) &&
            !(st instanceof DropColumnStep) &&
            !(st instanceof CreateIndexStep))
         {
            break;
         }
      }

      return false;
   }

   /**
    * Appends SQL for renaming an index to the SQL appender and upgrades the metadata.
    * @param step The rename index step.
    */
   protected void renameIndex(RenameIndexStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Index oldIndex = schema.getIndex(step.getOldName()).getTable().cloneTemporary()
                          .getIndex(step.getOldName());

      step.apply(m_state);

      renameIndex(step.getIndex(), oldIndex);
   }

   /**
    * Appends SQL for dropping an index to the SQL appender and upgrades the metadata.
    * @param step The drop index step.
    */
   protected void dropIndex(DropIndexStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Index index = schema.getIndex(step.getName());

      if (!index.isAspect())
      {
         if (index.getTable().isAspect())
         {
            Table[] tableArray = getSortedTables(m_state.getSchema());

            for (int i = 0, nCount = tableArray.length; i < nCount; ++i)
            {
               Table pointcut = tableArray[i];

               if (pointcut.hasAspect(index.getTable()))
               {
                  dropIndex(pointcut.getIndex(index.getName(pointcut)));
               }
            }
         }
         else
         {
            dropIndex(index);
         }
      }

      step.apply(m_state);
   }

   /**
    * Appends SQL for applying an index aspect to the SQL appender and upgrades the metadata.
    * @param step The apply index aspect step.
    */
   protected void applyIndexAspect(ApplyIndexAspectStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Lookup/*<Index, Index>*/ pointcutMap = new HashTab/*<Index, Index>*/(); // target -> current

      for (Iterator/*<Index>*/ itr = step.getPointcutIterator(schema.getIndexIterator());
           itr.hasNext();)
      {
         Index index = (Index)itr.next();

         pointcutMap.put(index, index.getTable().cloneTemporary().getIndex(index.getName()));
      }

      step.apply(m_state);

      for (Lookup.Iterator/*<Index, Index>*/ itr = pointcutMap.iterator(); itr.hasNext();)
      {
         itr.next();
         alterIndex((Index)itr.getKey(), (Index)itr.getValue());
      }
   }

   /**
    * Appends SQL for removing an index aspect to the SQL appender and upgrades the metadata.
    * @param step The remove index aspect step.
    */
   protected void removeIndexAspect(RemoveIndexAspectStep step)
   {
      RelationalSchema schema = m_state.getSchema();
      Index aspect = schema.getIndex(step.getAspectName());
      Lookup/*<Index, Index>*/ pointcutMap = new HashTab/*<Index, Index>*/(); // target -> current

      for (Iterator/*<Index>*/ itr = step.getPointcutIterator(schema.getIndexIterator());
           itr.hasNext();)
      {
         Index index = (Index)itr.next();

         if (index.hasAspect(aspect))
         {
            pointcutMap.put(index, index.getTable().cloneTemporary().getIndex(index.getName()));
         }
      }

      step.apply(m_state);

      for (Lookup.Iterator/*<Index, Index>*/ itr = pointcutMap.iterator(); itr.hasNext();)
      {
         itr.next();
         alterIndex((Index)itr.getKey(), (Index)itr.getValue());
      }
   }

   /**
    * Appends SQL to the SQL appender and upgrades the metadata.
    * @param step The SQL exec step.
    */
   protected void execSQL(ExecStep step)
   {
      step.apply(m_state);
      execSQL(step.getScriptHolder(), null);
   }

   /**
    * Appends SQL for altering differing columns to the SQL appender.
    * @param target The table to compare column definitions to. (not null)
    * @param current The table definition currently persisted in RDBMS. (not null)
    */
   protected void alterColumns(Table target, Table current)
   {
      if (target.getType() != Table.MANAGED)
      {
         return; // NOOP
      }

      Table template = current.cloneTemporary();

      // remove all indexes related to modified columns from template table
      for (int i = 0, nCount = target.getColumnCount(); i < nCount; ++i)
      {
         Column targetCol = target.getColumn(i);
         Column templateCol = template.getColumn(targetCol.getName());

         if (!isCompatible(targetCol, templateCol))
         {
            targetCol.copyTo(templateCol);

            for (int k = template.getIndexCount() - 1; k >= 0; --k) // backwards due to removal
            {
               Index index = template.getIndex(k);

               if (index.findIndexColumn(templateCol) != null)
               {
                  template.removeIndex(index);
               }
            }
         }
      }

      dropIndexes(template, current);

      // apply all column alterations
      for (int i = 0, nCount = target.getColumnCount(); i < nCount; ++i)
      {
         Column targetCol = target.getColumn(i);
         Column currentCol = current.getColumn(targetCol.getName());

         if (!isCompatible(targetCol, currentCol))
         {
            alterColumn(targetCol, currentCol);
            targetCol.copyTo(current.getColumn(targetCol.getName())); //column might change in alter
         }
      }

      createIndexes(target, current); // simpler to recreate all required indexes
   }

   /**
    * Appends SQL for creating a column to the SQL appender.
    * @param column The column to add.
    * @param bNullability True to create non-nullable columns as non-nullable.
    */
   protected void createColumn(Column column, boolean bNullability)
   {
      Table table = column.getTable();

      if (table.getType() != Table.MANAGED)
      {
         return; // nothing to do
      }

      StringBuffer buf = new StringBuffer(128);
      Column nullableColumn = column;

      if (!bNullability && !column.isNullable())
      {
         nullableColumn = (Column)column.clone();
         nullableColumn.setRequired(false, true);
         nullableColumn.setNullable(true);
      }

      buf.append("alter table ");
      appendTableName(buf, table);
      buf.append(getAddColumnToken());
      appendColumnDeclaration(buf, nullableColumn, true, false, getAddColumnToken());

      m_appender.appendSQL(buf.toString());
   }

   /**
    * Appends SQL for creating columns to the SQL appender.
    * @param target The table must contain all missing columns from target definition. (not null)
    * @param current The table definition currently persisted in RDBMS. (not null)
    * @param bNullability True to create non-nullable columns as non-nullable.
    */
   protected void createColumns(Table target, Table current, boolean bNullability)
   {
      if (target.getType() != Table.MANAGED)
      {
         return; // NOOP
      }

      for (int i = 0, nCount = target.getColumnCount(); i < nCount; ++i)
      {
         Column targetCol = target.getColumn(i);
         Column currentCol = current.findColumn(targetCol.getName());

         if (currentCol != null)
         {
            continue; // NOOP column already exists
         }

         currentCol = (Column)targetCol.clone();

         if (!bNullability && !currentCol.isNullable())
         {
            currentCol.setRequired(false, true); // otherwise isNullable returns false for required
            currentCol.setNullable(true); // do not set the nullibility flag
         }

         createColumn(currentCol, true); // true so createColumn doesn't have to clone()
         current.addColumn(currentCol);
      }
   }

   /**
    * Appends SQL for creating all non-inlined indexes for a given table to the SQL appender.
    * @param target The table must contain all missing indexes from target definition. (not null)
    * @param current The table definition currently persisted in RDBMS. (not null)
    */
   protected void createIndexes(Table target, Table current)
   {
      if (!isIndexable(target))
      {
         return; // NOOP
      }

      for (int i = 0, nCount = target.getIndexCount(); i < nCount; ++i)
      {
         Index targetInd = target.getIndex(i);
         Index currentInd = current.findIndex(targetInd.getName());

         if (currentInd == null && targetInd.getIndexColumnCount() > 0)
         {
            createIndex(targetInd);
            current.addIndex((Index)targetInd.clone());
         }
      }
   }

   /**
    * Make index of a table unavailable by its name in the least costly fashion. The table
    * will be marked for removal soon after this call therefore indexes are no longer nessesary.
    * Dropping the index is prefered, however if dropping the index requires recreation of the
    * table then renaming the index is just as satisfactory.
    * @param index The index to discard.
    * @param bUpdate Update the index's table definition with removed indexes.
    */
   protected void discardIndex(Index index, boolean bUpdate)
   {
      dropIndex(index);

      if (bUpdate)
      {
         index.getTable().removeIndex(index);
      }
   }

   /**
    * Appends SQL for dropping all extra indexes for a given table to the SQL appender.
    * @param target The table must not contain all extra columns from target definition. (not null)
    * @param current The table definition currently persisted in RDBMS. (not null)
    */
   protected void dropColumns(Table target, Table current)
   {
      if (target.getType() != Table.MANAGED)
      {
         return; // NOOP
      }

      for (int i = current.getColumnCount() - 1; i >= 0; --i) // backwards due to column removal
      {
         Column currentCol = current.getColumn(i);
         Column targetCol = target.findColumn(currentCol.getName());

         if (targetCol == null)
         {
            dropIndexes(currentCol, true);
            dropColumn(currentCol);
            current.removeColumn(currentCol);
         }
      }
   }

   /**
    * Appends SQL to drop indexes containing a given column to the SQL appender.
    * @param column Drop all indexes containing this column.
    * @param bUpdate Update the column's table definition with removed indexes.
    */
   protected void dropIndexes(Column column, boolean bUpdate)
   {
      Table table = column.getTable();

      for (int i = table.getIndexCount() - 1; i >= 0; --i) // backwards due to index removal
      {
         Index index = table.getIndex(i);

         if (index.findIndexColumn(column) != null)
         {
            dropIndex(index);

            if (bUpdate)
            {
               table.removeIndex(index);
            }
         }
      }
   }

   /**
    * Appends SQL for dropping all extra indexes for a given table to the SQL appender.
    * @param target The table must not contain all extra indexes from target definition. (not null)
    * @param current The table definition currently persisted in RDBMS. (not null)
    */
   protected void dropIndexes(Table target, Table current)
   {
      if (!isIndexable(target))
      {
         return; // NOOP
      }

      for (int i = current.getIndexCount() - 1; i >= 0; --i) // backwards due to index removal
      {
         Index currentInd = current.getIndex(i);
         Index targetInd = target.findIndex(currentInd.getName());

         // upgrade step applications leave indexes of size 0 which are invalid
         if (targetInd == null || targetInd.getIndexColumnCount() == 0)
         {
            dropIndex(currentInd);
            current.removeIndex(currentInd);
         }
      }
   }

   /**
    * Appends SQL to the SQL appender.
    * @param holder The source SQL script holder.
    * @param table The context table. Can be null.
    */
   protected void execSQL(SQLScriptHolder holder, Table table)
   {
      for (int i = 0, n = holder.getScriptCount(); i < n; ++i)
      {
         String sSQL = getExecSQL(holder.getScript(i), m_state.getSchema(), table);

         if (!StringUtil.isEmpty(sSQL))
         {
            m_appender.appendSQL(sSQL);
         }
      }
   }

   /**
    * Returns the expanded and formated SQL for a specific SQLScript.
    * @param script The script to get the SQL template from.
    * @param schema The relational schema that the script is destined for.
    * @param table The default table used during script template expansion. Can be null.
    * @return The expanded and formated SQL for the specified SQLScript.
    */
   protected String getExecSQL(SQLScript script, RelationalSchema schema, Table table)
   {
      return getExecSQL(script.getSQL(schema.getDataSource().getAdapter()), schema, table);
   }

   /**
    * Returns the expanded and formated SQL for a specific SQLScript.
    * @param script The script to get the SQL template from.
    * @param schema The relational schema that the script is destined for.
    * @param table The default table used during script template expansion. Can be null.
    * @return The expanded and formated SQL for the specified SQLScript.
    */
   public String getExecSQL(String sSQL, RelationalSchema schema, Table table)
   {
      if (StringUtil.isEmpty(sSQL))
      {
         return sSQL;
      }

      StringWriter writer = new StringWriter(sSQL.length());

      try
      {
         IOUtil.copy(writer, getSQLSubstReader(new StringReader(sSQL), schema, table));
      }
      catch (IOException e)
      {
         ObjUtil.rethrow(e);
      }

      return formatSQL(writer.toString().trim());
   }

   /**
    * Test if columns are compatible by comparing all column RDBMS properties except name and table.
    * @param left The first column.
    * @param right The second column.
    */
   protected boolean isCompatible(Column left, Column right)
   {
      if (left.getAllocation() != right.getAllocation() ||
          left.getType() != right.getType() ||
          left.isNullable() != right.isNullable())
      {
         return false;
      }

      if (left.getType() == Primitive.BOOLEAN)
      {
         return true; // skip Precision/Scale comparison for boolean since some DBs use integers
      }

      if (left.getType() == Primitive.DECIMAL) // evaluate default precision for decimals
      {
         int nDecPrec = m_adapter.getMaxDecimalPrecision();

         return left.getPrecision(nDecPrec) == right.getPrecision(nDecPrec) &&
                left.getScale(nDecPrec) == right.getScale(nDecPrec);
      }

      return left.getPrecision() == right.getPrecision() && left.getScale() == right.getScale();
   }

   /**
    * Creates a new instance of an SQLSubstReader.
    * @param reader The reader to wrap.
    * @param schema The relational schema.
    * @param table The context table. Can be null.
    */
   protected SQLSubstReader getSQLSubstReader(Reader reader, RelationalSchema schema, Table table)
   {
      return new SQLSubstReader(reader, schema, table)
      {
         protected StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray)
         {
            return SQLSchemaManager.this.appendConcatenate(buf, argArray);
         }

         protected StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField)
         {
            return SQLSchemaManager.this.appendTSExtract(buf, sTS, nField);
         }

         protected StringBuffer appendTSIncrement(
            StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField)
         {
            return SQLSchemaManager.this.appendTSIncrement(buf, sTS, sDelta, nField);
         }

         protected String getQuotedTableName(Table table)
         {
            return getTableName(table);
         }

         protected String getQuotedObjectName(SQLObject object)
         {
            StringBuffer buf = new StringBuffer(64);

            m_adapter.appendRelationalObject(buf, object, getOwner(), null, true);

            return buf.toString();
         }

         protected String getQuotedIndexName(Index index)
         {
            return getIndexName(index, isPrimaryKey(index), true);
         }

         protected String getQuotedKeyword(String sName)
         {
            if (sName != null && m_adapter.isKeyword(sName))
            {
               StringBuffer buf = new StringBuffer(sName.length() + 2);

               m_adapter.appendQuoted(buf, sName);

               return buf.toString();
            }
            
            return sName;
         }

         protected String getQuotedOwnerName()
         {
            return quote(getOwner());
         }

         protected String getQuotedRoleName()
         {
            return quote(getRole(m_schema));
         }

         protected boolean isColumnCaseInsensitive(Column column)
         {
            return isCaseInsensitive(column);
         }

         protected String getBinaryLiteral(String sHex)
         {
            return getLiteral(Primitive.BINARY, Binary.parse(sHex));
         }

         protected String getGUID()
         {
            return getGUIDExpr();
         }

         protected String getNow()
         {
            return getNowExpr();
         }

         protected String getSysPublicId()
         {
            return getLiteral(Primitive.BINARY, SYS_PUBLIC_GUID);
         }

         protected String getSysUserId()
         {
            return getLiteral(Primitive.BINARY, SYS_USER_GUID);
         }

         protected String getSysUserAlias()
         {
            return getLiteral(Primitive.STRING, "nexjsa");
         }

         protected String getLiteral(Primitive type, Object value)
         {
            StringBuffer buf = new StringBuffer(36);

            m_adapter.appendLiteral(buf, type, type.convert(value));

            return buf.toString();
         }
      };
   }

   /**
    * @return An expression returning a new GUID.
    */
   protected abstract String getGUIDExpr();

   /**
    * @return An expression returning the current UTC timestamp.
    */
   protected abstract String getNowExpr();

   /**
    * Appends the SQL statements for a complete stand-alone upgrade of a schema to the SQL appender.
    * @param schema The relational schema.
    */
   public void upgrade(RelationalSchema schema)
   {
      upgrade(schema, null);
   }

   /**
    * Appends the SQL statements for a complete stand-alone upgrade of a schema to the SQL appender.
    * @param schema The relational schema.
    * @param sVersion The starting version of the upgrade (null == first available version).
    */
   public void upgrade(RelationalSchema schema, String sVersion)
   {
      upgrade(schema, schema.getMetadata().getUpgrade("Main"), sVersion);
   }

   /**
    * Appends the SQL statements for a complete stand-alone upgrade of a schema to the SQL appender.
    * @param schema The relational schema.
    * @param upgrade The upgrade collection to use (not null).
    * @param sVersion The starting version of the upgrade (null == first available version).
    */
   public void upgrade(RelationalSchema schema, Upgrade upgrade, String sVersion)
   {
      init(schema);

      if (schema.getVersionTable() == null)
      {
         throw new MetadataException("err.meta.upgrade.sql.versionTable",
            new Object[]{schema.getDataSource().getName()});
      }

      StringBuffer buf = new StringBuffer(256);
      SQLAppender outerAppender = m_appender;
      SQLAppender innerAppender = getDynamicSQLAppender(buf);
      Metadata metadata = schema.getMetadata();
      VersionUpgrade firstVersion = (sVersion == null)
                                  ? upgrade.getFirstVersion() : upgrade.getVersion(sVersion);
      Lookup/*<Object, UpgradeState>*/ stateMap = Upgrade.getInitialState(firstVersion);
      SchemaVersion version = new SchemaVersion();

      upgrade.validate(metadata, null);
      version.setNamespace(metadata.getNamespace());
      version.setUpgradable(true);
      appendUpgradeInitialize(buf, schema);

      if (buf.length() > 0) // don't want to output end-of-statement character if there's nothing in buffer
      {
         m_appender.appendSQL(buf.toString());
         buf.setLength(0);
      }

      // skip all upgrade versions not applicable to schema's DataSourceAdapter
      if (sVersion == null)
      {
         firstVersion =
            findFirstVersion(firstVersion, stateMap, (RelationalDatabase)schema.getDataSource());
      }

      for (VersionUpgrade u = firstVersion; u != null; u = u.getNext())
      {
         if (u.getName() == null)
         {
            u.apply(Upgrade.getState(stateMap, u));
         }
         else
         {
            version.setVersion(u.getName());
            version.setStep(UPGRADE_END_STEP);

            SchemaVersion prev = (SchemaVersion)version.clone();

            version.setStep((u instanceof ScriptUpgrade && ((ScriptUpgrade)u).getFunction() == null ||
               u instanceof RelationalSchemaUpgrade &&
                  ((RelationalSchemaUpgrade)u).getDataSource().getSchema() != schema ||
               u instanceof LabelUpgrade) ? UPGRADE_END_STEP : 0);

            if (u.getPrev() != null && u.getPrev().getName() != null)
            {
               prev.setVersion(u.getPrev().getName());
               appendUpgradeStepStart(buf, schema, prev);
               appendUpgradeStepEnd(buf, schema, version, prev, null);
               m_appender.appendSQL(buf.toString());
               buf.setLength(0);
            }

            if (u instanceof RelationalSchemaUpgrade)
            {
               RelationalSchemaUpgrade rsu = (RelationalSchemaUpgrade)u;

               if (rsu.getDataSource().getSchema() == schema)
               {
                  RelationalSchemaUpgradeState state = (RelationalSchemaUpgradeState)Upgrade.getState(stateMap, u);

                  do
                  {
                     prev = (SchemaVersion)version.clone();
                     appendUpgradeStepStart(buf, state.getSchema(), version);

                     try
                     {
                        m_appender = innerAppender;
                        upgrade(rsu, state, version);
                     }
                     finally
                     {
                        m_appender = outerAppender;
                     }

                     appendUpgradeStepEnd(buf, state.getSchema(), version, prev, null);

                     m_appender.appendSQL(buf.toString());
                     buf.setLength(0);
                  }
                  while (version.getStep() >= 0);

                  continue;
               }
            }
            else if (u instanceof ScriptUpgrade &&
               ((ScriptUpgrade)u).getFunction() != null)
            {
               appendUpgradeStepStart(buf, schema, version);
               appendPrint(buf, "Please run the Scheme code for upgrade version \"" + u.getName() +
                  "\" from the console with the same repository version");
               buf.append(SysUtil.LINE_SEP);
               appendUpgradeStepEnd(buf, schema, null, version, null);
               m_appender.appendSQL(buf.toString());
               buf.setLength(0);
            }
         }
      }

      version.setVersion(metadata.getVersion());
      version.setStep(UPGRADE_END_STEP);
      appendUpgradeStepStart(buf, schema, version);
      appendPrint(buf, "Database is up to date");
      buf.append(SysUtil.LINE_SEP);
      appendUpgradeStepEnd(buf, schema, null, version, "Database update failed");
      m_appender.appendSQL(buf.toString());
      buf.setLength(0);

      appendUpgradeFinalize(buf, schema);

      if (buf.length() > 0) // don't want to output end-of-statement character if there's nothing in buffer
      {
         m_appender.appendSQL(buf.toString());
         buf.setLength(0);
      }
   }

   /**
    * Appends the SQL for initializing upgrade procedure, (before any steps), to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    */
   protected void appendUpgradeInitialize(StringBuffer buf, RelationalSchema schema)
   {
   }

   /**
    * Appends the SQL for starting an upgrade step to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    * @param version The current upgrade version.
    */
   protected abstract void appendUpgradeStepStart(StringBuffer buf, RelationalSchema schema, SchemaVersion version);

   /**
    * Appends the SQL for ending an upgrade step to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    * @param version The next upgrade version.
    * @param prev The previous version.
    * @param sFailMsg The failure message if step was not executed (null == none).
    */
   protected abstract void appendUpgradeStepEnd(StringBuffer buf,
      RelationalSchema schema, SchemaVersion version, SchemaVersion prev, String sFailMsg);

   /**
    * Appends the SQL for finalizing upgrade procedure, (after all steps), to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    */
   protected void appendUpgradeFinalize(StringBuffer buf, RelationalSchema schema)
   {
   }

   /**
    * Appends the SQL for printing a message to a string buffer.
    * @param buf The destination string buffer.
    * @param sMsg The message to print.
    */
   protected abstract void appendPrint(StringBuffer buf, String sMsg);

   /**
    * Appends the SQL from clause for selecting the version record to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    * @param version The current upgrade version.
    */
   protected void appendVersionTableFrom(StringBuffer buf, RelationalSchema schema, SchemaVersion version)
   {
      buf.append(" from ");
      appendTableName(buf, schema.getVersionTable());
      buf.append(" where namespace = ");
      m_adapter.appendLiteral(buf, Primitive.STRING, version.getNamespace());
      buf.append(" and version = ");
      m_adapter.appendLiteral(buf, Primitive.STRING, version.getVersion());
      buf.append(" and step = ");
      buf.append(version.getStep());
   }

   /**
    * Appends to a string buffer the SQL for inserting requested version into the version table.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    * @param version The schema version.
    * @param bLoaded True if the initial (seed) data has been loaded @see SysVersion.meta.
    */
   protected void appendVersionTableInsert(StringBuffer buf,
      RelationalSchema schema, SchemaVersion version, boolean bLoaded)
   {
      buf.append("insert into ");
      appendTableName(buf, schema.getVersionTable());
      buf.append("(namespace, version, step, upgradable, test, loaded) values (");
      m_adapter.appendLiteral(buf, Primitive.STRING, version.getNamespace());
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.STRING, version.getVersion());
      buf.append(", ");
      buf.append(version.getStep());
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.valueOf(version.isUpgradable()));
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.valueOf(version.isTest()));
      buf.append(", ");
      m_adapter.appendLiteral(buf, Primitive.BOOLEAN, Boolean.valueOf(bLoaded));
      buf.append(')');
   }

   /**
    * Appends the SQL for updating the version table to a string buffer.
    * @param buf The destination string buffer.
    * @param schema The schema object.
    * @param version The next upgrade version.
    * @param prev The previous version.
    */
   protected void appendVersionTableUpdate(StringBuffer buf, RelationalSchema schema, SchemaVersion version, SchemaVersion prev)
   {
      buf.append("update ");
      appendTableName(buf, schema.getVersionTable());
      buf.append(" set step = ");
      buf.append(version.getStep());

      if (!version.getVersion().equals(prev.getVersion()))
      {
         buf.append(", version = ");
         m_adapter.appendLiteral(buf, Primitive.STRING, version.getVersion());
      }

      buf.append(" where namespace = ");
      m_adapter.appendLiteral(buf, Primitive.STRING, version.getNamespace());
      buf.append(" and version = ");
      m_adapter.appendLiteral(buf, Primitive.STRING, prev.getVersion());
      buf.append(" and step = ");
      buf.append(prev.getStep());
   }

   /**
    * Determines if the column requires special case-insensitive handling.
    * @param column The column of which to compute ifci.
    * @return True if the column requires special case-insensitive handling.
    */
   protected boolean isCaseInsensitive(Column column)
   {
      return false;
   }

   /**
    * Returns the role that is granted access to the schema objects.
    * @param schema The relational schema.
    */
   protected String getRole(RelationalSchema schema)
   {
      if (schema.getRoleName() != null)
      {
         return schema.getRoleName();
      }

      return getDefaultRole();
   }

   /**
    * Computes the tablespace name of a table.
    * @param table The table.
    * @return The tablespace name. Can be null.
    */
   protected String getTablespace(Table table)
   {
      String sName = table.getTablespaceName();

      if (sName == null)
      {
         sName = ((RelationalDatabase)table.getSchema().getDataSource()).getTablespaceName();
      }

      if (sName == null)
      {
         sName = table.getSchema().getTablespaceName();
      }

      if (DEFAULT_TABLESPACE.equals(sName))
      {
         return null;
      }

      if (sName == null)
      {
         sName = getDefaultTablespace();
      }

      return sName;
   }

   /**
    * Computes the index tablespace name of an index.
    * @param index The index.
    * @return The index tablespace name. Can be null.
    */
   protected String getIndexspace(Index index)
   {
      Table table = index.getTable();

      String sName = table.getIndexspaceName();

      if (sName == null)
      {
         sName = table.getTablespaceName();
      }

      if (sName == null)
      {
         sName = ((RelationalDatabase)table.getSchema().getDataSource()).getIndexspaceName();
      }

      if (sName == null)
      {
         sName = ((RelationalDatabase)table.getSchema().getDataSource()).getTablespaceName();
      }

      if (sName == null)
      {
         sName = table.getSchema().getIndexspaceName();
      }

      if (sName == null)
      {
         sName = table.getSchema().getTablespaceName();
      }

      if (DEFAULT_TABLESPACE.equals(sName))
      {
         return null;
      }

      if (sName == null)
      {
         sName = getDefaultIndexspace();
      }

      if (sName == null)
      {
         sName = getDefaultTablespace();
      }

      return sName;
   }

   /**
    * Computes the long column tablespace name of a table.
    * @param table The table.
    * @return The long column tablespace name. Can be null.
    */
   protected String getLongspace(Table table)
   {
      String sName = table.getLongspaceName();

      if (sName == null)
      {
         sName = table.getTablespaceName();
      }

      if (sName == null)
      {
         sName = ((RelationalDatabase)table.getSchema().getDataSource()).getLongspaceName();
      }

      if (sName == null)
      {
         sName = ((RelationalDatabase)table.getSchema().getDataSource()).getTablespaceName();
      }

      if (sName == null)
      {
         sName = table.getSchema().getLongspaceName();
      }

      if (sName == null)
      {
         sName = table.getSchema().getTablespaceName();
      }

      if (DEFAULT_TABLESPACE.equals(sName))
      {
         return null;
      }

      if (sName == null)
      {
         sName = getDefaultLongspace();
      }

      if (sName == null)
      {
         sName = getDefaultTablespace();
      }

      return sName;
   }

   /**
    * Appends a suffix to a string buffer.
    * @param sPrefix The prefix to append if sSuffix is not null. Can be null.
    * @param sSuffix The suffix to append. Can be null.
    */
   protected void appendSuffix(StringBuffer buf, String sPrefix, String sSuffix)
   {
      if (sSuffix != null)
      {
         if (sPrefix != null)
         {
            buf.append(sPrefix);
         }

         buf.append(sSuffix);
      }
   }

   /**
    * Computes the index page fill factor for a given index.
    * @param index The index.
    * @return The index page fill factor, 0-100.
    */
   protected byte getIndexFill(Index index)
   {
      byte nFill = index.getFill();

      if (nFill < 0)
      {
         nFill = index.getTable().getSchema().getIndexFill();
      }

      return nFill;
   }

   /**
    * Quotes reserved words.
    * @param sName The string to quote.
    * @return The quoted string (or the original one, if not reserved).
    */
   protected String quote(String sName)
   {
      if (sName != null && m_adapter.isKeyword(sName))
      {
         StringBuffer buf = new StringBuffer(sName.length() + 2);

         m_adapter.appendQuoted(buf, sName);

         return buf.toString();
      }

      return sName;
   }

   /**
    * Appends the SQL statements from a string to the SQL appender.
    * @param sName The resource name, relative to ./etc/. Can be null.
    * @param schema The relational schema.
    */
   protected void script(String sName, RelationalSchema schema)
   {
      if (sName != null)
      {
         InputStream istream = null;
         StringWriter writer = new StringWriter(1024);

         try
         {
           istream = URLUtil.openResource(getClass(), "etc/" + sName);
           IOUtil.copy(writer, getSQLSubstReader(new InputStreamReader(istream, XMLUtil.ENCODING), schema, null));
         }
         catch (IOException e)
         {
            ObjUtil.rethrow(e);
         }
         finally
         {
            IOUtil.close(istream);
         }

         appendSQLScript(writer.toString());
      }
   }

   /**
    * Template method for validating/formatting of size arguments used in DB creation templates.
    * @param sSize The value to validate/format.
    * @return The formatted value valid for the corresponding DB.
    * @throws NumberFormatException On error parsing argument.
    * @throws ValidationException On error validation argument.
    */
   protected String formatByteSize(String sSize) throws NumberFormatException, ValidationException
   {
      if (sSize == null)
      {
         return null;
      }

      long lSize = parseByteValue(sSize);

      if (lSize < 0)
      {
         throw new ValidationException("err.persistence.valueRange", new Object[]{sSize});
      }

      if ((lSize & 0x3fffffffL) == 0) // GB
      {
         return Long.toString(lSize >> 30) + "G";
      }

      if ((lSize & 0xfffffL) == 0) // MB
      {
         return Long.toString(lSize >> 20) + "M";
      }

      if ((lSize & 0x3ffL) == 0) // KB
      {
         return Long.toString(lSize >> 10) + "K";
      }

      return Long.toString(lSize);
   }

   /**
    * Template method for validating/formatting of percent arguments in DB creation templates.
    * @param sPercent The value to validate/format.
    * @return The formatted value valid for the corresponding DB.
    * @throws NumberFormatException On error parsing argument.
    * @throws ValidationException On error validation argument.
    */
   protected String formatPercent(String sPercent) throws NumberFormatException, ValidationException
   {
      if (sPercent == null)
      {
         return null;
      }

      int nPercent = Integer.parseInt(sPercent);

      if (nPercent < 0 || nPercent > 100)
      {
         throw new ValidationException("err.persistence.valueRange", new Object[]{sPercent});
      }

      return Integer.toString(nPercent);
   }

   /**
    * Template method for formatting of an SQL statement.
    * @param sSQL The statement to format.
    * @return The formatted statement.
    */
   protected String formatSQL(String sSQL)
   {
      return sSQL;
   }

   /**
    * @return The SQL statement separator.
    */
   public abstract String getSeparator();

   /**
    * @return The name of the SQL script to create the misc objects,
    * relative to ./scripts/. Can be null.
    */
   protected abstract String getCreateEtcScriptName();

   /**
    * @return The name of the SQL script to drop the misc objects,
    * relative to ./scripts/. Can be null.
    */
   protected abstract String getDropEtcScriptName();

   /**
    * @return The name of the SQL template script used for setting up a new database,
    *         relative to ./scripts/. can be null.
    */
   protected abstract String getSetupEtcScriptName();

   /**
    * Wraps a string buffer with an appender for generating dynamic SQL.
    * @param buf The destination string buffer.
    * @return The dynamic SQL appender.
    */
   protected abstract SQLAppender getDynamicSQLAppender(StringBuffer buf);

   /**
    * Gets a sorted array of tables from a schema.
    * @param schema The relational schema.
    * @return The sorted array of tables;
    */
   protected static Table[] getSortedTables(RelationalSchema schema)
   {
      Table[] tableArray = new Table[schema.getTableCount()];
      int i = 0;

      for (Iterator itr = schema.getTableIterator(); itr.hasNext();)
      {
         tableArray[i++] = (Table)itr.next();
      }

      Arrays.sort(tableArray, TABLE_COMPARATOR);

      return tableArray;
   }

   /**
    * Go through all tables in the schema and add any table definitions that generate drop scripts
    * not matching any table in the map.
    * @param tableMap MultiMap to add to.
    * @param schema The schema to get table definitions from.
    */
   protected void addDropTable(Lookup/*<String, List<Table>>*/ tableMap, RelationalSchema schema)
   {
      for (Iterator/*<Table>*/ itr = schema.getTableIterator(); itr.hasNext();)
      {
         int nTable = 0;
         Table newTable = (Table)itr.next();
         String sTable = getTableName(newTable);
         List/*<Table>*/ list = (ArrayList)tableMap.get(sTable);
         Table mapTable = (list == null) ? null : (Table)list.get(nTable);

         // go through all similarly named tables and see if drop script will match
         while (mapTable != null)
         {
            Table successor = (mapTable.getType() == newTable.getType())
                            ? getDropSuccessor(mapTable, newTable) : null;

            if (successor == null)
            {
               mapTable = (list.size() > ++nTable) ? (Table)list.get(nTable) : null;

               continue; // no common representation for this offset, try next table
            }

            if (successor != mapTable) // more generic representation of table
            {
               if (successor == newTable) // successor same object as the table in schema
               {
                  mapTable = null; // force clone+replacement of table in tableMap
               }
               else // put successor into map directly since it is not same object as in schema
               {
                  list.set(nTable, successor);
               }
            }

            break;
         }

         if (mapTable == null) // newTable not in map or newTable's drop script differs from others
         {
            mapTable = newTable.cloneTemporary();

            if (list == null)
            {
               list = new ArrayList/*<Table>*/();
               tableMap.put(sTable, list);
            }

            if (list.size() > nTable)
            {
               list.set(nTable, mapTable);
            }
            else
            {
               list.add(mapTable);
            }
         }
      }
   }

   /**
    * Determine the table definition that would generate drop statements aplicable to both tables.
    * This method should most likely be overidden by child implementations.
    * @param left The first table definition to compare (not null).
    * @param right The second table definition to compare (not null).
    * @return The table definition that would generate drop statements applicable to both tables,
    *         the table definition may be an entirely different object, or null if no single
    *         definition can generate drop statements applicable to both tables.
    */
   protected Table getDropSuccessor(Table left, Table right)
   {
      return getDropSuccessor(left, right, (byte)-1);
   }

   /**
    * Determinte the table definition that would generate drop statements aplicable to both tables
    * taking into consideration isIdentityKeyGenerator() and presence of individual indexes of the
    * type specified by nIndexType.
    * This implementation is used by Oracle/DB2 for getDropEquivalent().
    * @param left The first table definition to compare (not null).
    * @param right The second table definition to compare (not null).
    * @param nIndexType Prefer tables having the superset of this index type,
    *                   one of Index.* constants (nIndexType < 0 == ignore).
    * @return The table definition that would generate drop statements applicable to both tables,
    *         the table definition may be an entirely different object, or null if no single
    *         definition can generate drop statements applicable to both tables.
    */
   protected Table getDropSuccessor(Table left, Table right, byte nIndexType)
   {
      assert left.getName().equals(right.getName());
      assert left.getType() == right.getType();

      if (hasExtraIndex(left, right, nIndexType))
      {
          // both left/right have indexes not in the other || left preferred but not generator
         if (hasExtraIndex(right, left, nIndexType) ||
             (!left.isIdentityKeyGenerator() && right.isIdentityKeyGenerator()))
         {
            return null; // neither left or right is a superset of the other
         }

         return left;
      }

      if (hasExtraIndex(right, left, nIndexType))
      {
         // right preferred but not generator
         if (left.isIdentityKeyGenerator() && !right.isIdentityKeyGenerator())
         {
            return null; // neither left or right is a superset of the other
         }
      }
      else
      {
         if (left.isIdentityKeyGenerator() || !right.isIdentityKeyGenerator())
         {
            return left;
         }
      }

      return right;
   }

   /**
    * Determine if there are any indexes of type nIndexType present in table that are not in sub.
    * @param table The table wich to check for indexes not in sub.
    * @param sub The subset of indexes that must exist in table.
    * @param nIndexType The type of index to match.
    * @return If table has indexes of type nIndexType that are not in sub.
    */
   protected boolean hasExtraIndex(Table table, Table sub, byte nIndexType)
   {
      if (nIndexType < 0)
      {
         return false; // nIndexType not one of Index.* constants, hence invalid/absent
      }

      for (int i = 0, nCount = table.getIndexCount(); i < nCount; ++i)
      {
         Index index = table.getIndex(i);

         if (index.getType() == nIndexType && sub.findIndex(index.getName()) == null)
         {
            return true;
         }
      }

      return false;
   }

   // inner classes

   /**
    * Class for parsing database creation templates.
    */
   protected static class DatabaseTemplateSubstReader extends SQLTemplateSubstReader
   {
      /**
       * The path separator to use with ${path:...} macros.
       */
      protected char m_chFileSep;

      /**
       * The map containing defaults used when values not availbale.
       */
      protected PropertyMap m_defaultsMap;

      /**
       * The schema object to query for metadat state.
       */
      protected RelationalSchema m_schema;

      /**
       * The map containing values to return.
       */
      protected PropertyMap m_valuesMap;

      /**
       * Constructor.
       * @param input The reader to query for input.
       * @param schema The schema object to query for metadat state (not null).
       * @param valuesMap The map containing values to return (not null).
       * @param defaultsMap The map containing defaults used when values not availbale (not null).
       * @param chFileSep The path separator to use with ${path:...} macros.
       */
      public DatabaseTemplateSubstReader(Reader input, RelationalSchema schema,
         PropertyMap valuesMap, PropertyMap defaultsMap, char chFileSep)
      {
         super(input);
         assert schema != null && valuesMap != null && defaultsMap != null;
         m_defaultsMap = defaultsMap;
         m_schema = schema;
         m_valuesMap = valuesMap;
         m_chFileSep = chFileSep;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLTemplateSubstReader#getValue(java.lang.String, java.lang.String)
       * @throws RuntimeException On IO error during sub-template evaluation.
       * @throws IOException On IO error during evaluation.
       */
      protected Object findValue(String sKey, String sDefaultValue)
         throws RuntimeException, IOException
      {
         Object value = substitute(sKey, sDefaultValue);

         if (value == null)
         {
            value = sDefaultValue; // script default takes precedence over defaultsMap
         }

         if (value == null)
         {
            value = m_defaultsMap.findValue(sKey); // check defaults
         }

         return value;
      }

      /**
       * @see nexj.core.meta.persistence.sql.SQLTemplateSubstReader#getValue(java.lang.String, java.lang.String)
       * @throws LookupException On key lookup failure.
       * @throws RuntimeException On IO error during sub-template evaluation.
       * @throws IOException On IO error during evaluation.
       */
      protected Object getValue(String sKey, String sDefaultValue)
         throws LookupException, RuntimeException, IOException
      {
         Object value = findValue(sKey, sDefaultValue);

         if (value != null)
         {
            return value;
         }

         if (m_valuesMap.hasValue(sKey) || m_defaultsMap.hasValue(sKey))
         {
            throw new LookupException("err.meta.persistence.sql.undefined",
                                      new Object[]{sKey}); // no value available
         }

         // below here key is unsupported (build a list of valid keys for error reporting)
         Set/*<String>*/ keySet = new HashHolder/*<String>*/(m_valuesMap.getValueCount());

         for (PropertyIterator itr = m_defaultsMap.getIterator(); itr.hasNext();)
         {
            keySet.add(itr.next());
         }

         for (PropertyIterator itr = m_valuesMap.getIterator(); itr.hasNext();)
         {
            keySet.add(itr.next());
         }

         Object[] keyArray = keySet.toArray(new String[keySet.size()]);

         Arrays.sort(keyArray); // sort keys for readability

         throw new LookupException("err.meta.persistence.sql.variable",
                                   new Object[]{sKey, keyArray}); // unknown key
      }

      /**
       * Add key/value pair to map if key is not null.
       * @param map The map to add to (not null).
       * @param key The key to add.
       * @param value The value to add.
       * @return The previous value corresponding to this key (or null if none).
       */
      private static Object putNotNull(Lookup/*<Object, Object>*/ map, Object key, Object value)
      {
         if (key != null)
         {
            key = map.put(key, value);
         }

         return key;
      }

      /**
       * Evaluates potential macros.
       * @param sKey The name of the macro or variable name to get the value for.
       * @param sValue The macro body or the default value to use if none is available.
       * @return The substitution value.
       * @throws RuntimeException On IO error during sub-template evaluation.
       * @throws IOException On sub-expression evaluation error.
       */
      private Object substitute(String sKey, String sValue) throws RuntimeException, IOException
      {
         if ("for-each".equals(sKey) && sValue != null)
         {
            int nPos = sValue.indexOf(':');
            String sCollection = (nPos < 0) ? sValue : sValue.substring(0, nPos);

            // supported collections
            if ("indexspace".equals(sCollection) ||
                "longspace".equals(sCollection) ||
                "tablespace".equals(sCollection))
            {
               Lookup/*<Object, String>*/ valueMap =
                  new HashTab/*<Object, String>*/(m_schema.getTableCount() + 1);

               putNotNull(valueMap, findValue("indexspace", null), "indexspace"); // add default

               for (Iterator/*<Table>*/ itr = m_schema.getTableIterator(); itr.hasNext();)
               {
                  putNotNull(valueMap, ((Table)itr.next()).getIndexspaceName(), "indexspace");
               }

               putNotNull(valueMap, findValue("longspace", null), "longspace"); // add default

               for (Iterator/*<Table>*/ itr = m_schema.getTableIterator(); itr.hasNext();)
               {
                  putNotNull(valueMap, ((Table)itr.next()).getLongspaceName(), "longspace");
               }

               putNotNull(valueMap, findValue("tablespace", null), "tablespace"); // add default

               for (Iterator/*<Table>*/ itr = m_schema.getTableIterator(); itr.hasNext();)
               {
                  putNotNull(valueMap, ((Table)itr.next()).getTablespaceName(), "tablespace");
               }

               List/*<String>*/ valueList = new ArrayList/*<String>*/(valueMap.size());

               for (Lookup.Iterator/*<Object, String>*/ itr = valueMap.iterator(); itr.hasNext();)
               {
                  if (itr.next() instanceof String && itr.getValue().equals(sCollection))
                  {
                     valueList.add(itr.getKey());
                  }
               }

               Collections.sort(valueList, String.CASE_INSENSITIVE_ORDER); // force identical order

               StringBuffer buf = new StringBuffer(64);
               Object sOriginal = m_valuesMap.getValue(sCollection);
               String sTemplate = sValue.substring((nPos < 0) ? sValue.length() : nPos + 1);

               try
               {
                  // for each object in collection substitute its value
                  for (int i = 0, nCount = valueList.size(); i < nCount; ++i)
                  {
                     m_valuesMap.setValue(sCollection, valueList.get(i));
                     buf.append(substitute(sTemplate));
                  }
               }
               finally
               {
                  m_valuesMap.setValue(sCollection, sOriginal);
               }

               return buf;
            }
         }
         else if ("iftest".equals(sKey) && sValue != null)
         {
            // do not evaluate sub expression for non-test environments
            return (m_schema.getMetadata().isTestEnvironment()) ? substitute(sValue) : "";
         }
         else if ("path".equals(sKey) && sValue != null)
         {
            return substitute(sValue.replace('/', m_chFileSep).replace('\\', m_chFileSep));
         }

         return m_valuesMap.findValue(sKey);
      }
   }

   /**
    * Interface implemented by components processing SQL statements.
    */
   public interface SQLAppender
   {
      /**
       * Appends one complete SQL statement.
       * @param sSQL The SQL statement, without any statement delimiters like ; or GO.
       */
      void appendSQL(String sSQL);
   };

   /**
    * SQL appender adapter to a character stream writer.
    */
   public class SQLWriterAppender implements SQLAppender
   {
      /**
       * The writer.
       */
      protected Writer m_writer;

      /**
       * Constructs the appender.
       * @param writer The character stream writer.
       */
      public SQLWriterAppender(Writer writer)
      {
         m_writer = writer;
      }

      /**
       * @see nexj.core.persistence.sql.SQLSchemaManager.SQLAppender#appendSQL(java.lang.String)
       */
      public void appendSQL(String sSQL)
      {
         try
         {
            m_writer.write(sSQL);
            m_writer.write(getSeparator());
         }
         catch (IOException e)
         {
            ObjUtil.rethrow(e);
         }
      }
   }

   /**
    * SQL appender adapter to an output file.
    */
   public class SQLFileAppender extends SQLWriterAppender
   {
      // attributes

      /**
       * True if the appending has failed.
       */
      protected boolean m_bFailed;

      // associations

      /**
       * The output file. Null for System.out.
       */
      protected File m_file;

      // constructors

      /**
       * Constructs the appender.
       * @param manager The corresponding schema manager.
       * @param file The output file. Null for System.out.
       */
      public SQLFileAppender(File file)
      {
         super(null);
         m_file = file;
      }

      // operations

      /**
       * @see nexj.core.persistence.sql.SQLSchemaManager.SQLWriterAppender#appendSQL(java.lang.String)
       */
      public void appendSQL(String sSQL)
      {
         try
         {
            if (m_writer == null)
            {
               OutputStream ostream;

               if (m_file == null)
               {
                  ostream = System.out;
               }
               else
               {
                  ostream = new FileOutputStream(m_file);
               }

               m_writer = new OutputStreamWriter(new BufferedOutputStream(ostream), XMLUtil.ENCODING);
            }

            super.appendSQL(sSQL);
         }
         catch (Throwable t)
         {
            m_bFailed = true;
            ObjUtil.rethrow(t);
         }
      }

      /**
       * Closes the underlying writer.
       */
      public void close()
      {
         try
         {
            if (m_writer != null)
            {
               try
               {
                  m_writer.close();
                  m_writer = null;
               }
               catch (Throwable t)
               {
                  m_bFailed = true;
                  ObjUtil.rethrow(t);
               }
            }
         }
         finally
         {
            if (m_bFailed)
            {
               m_file.delete();
            }
         }
      }
   }

   /**
    * SQL appender adapter to an SQL connection.
    */
   public static class SQLConnectionAppender implements SQLAppender
   {
      // attributes

      /**
       * The log level, one of the Logger.* constants.
       */
      protected int m_nLogLevel = Logger.DEBUG;

      /**
       * True to ignore the SQL exceptions.
       */
      protected boolean m_bSafe;

      // associations

      /**
       * The SQL connection.
       */
      protected Connection m_connection;

      // constructors

      /**
       * Constructs the appender.
       * @param connection The SQL connection.
       */
      public SQLConnectionAppender(Connection connection)
      {
         m_connection = connection;
      }

      /**
       * Constructs the appender.
       * @param connection The SQL connection.
       * @param bSafe True to ignore the SQL exceptions.
       */
      public SQLConnectionAppender(Connection connection, boolean bSafe)
      {
         m_connection = connection;
         m_bSafe = bSafe;
      }

      // operations

      /**
       * Sets the log level, one of the Logger.* constants.
       * @param nLogLevel The log level, one of the Logger.* constants to set.
       */
      public void setLogLevel(int nLogLevel)
      {
         m_nLogLevel = nLogLevel;
      }

      /**
       * @return The log level, one of the Logger.* constants.
       */
      public int getLogLevel()
      {
         return m_nLogLevel;
      }

      /**
       * Sets the SQL exception suppression flag.
       * @param bSafe The SQL exception suppression flag to set.
       */
      public void setSafe(boolean bSafe)
      {
         m_bSafe = bSafe;
      }

      /**
       * @return The SQL exception suppression flag.
       */
      public boolean isSafe()
      {
         return m_bSafe;
      }

      /**
       * @see nexj.core.persistence.sql.SQLSchemaManager.SQLAppender#appendSQL(java.lang.String)
       */
      public void appendSQL(String sSQL)
      {
         Statement stmt = null;

         try
         {
            s_logger.log(m_nLogLevel, sSQL);
            stmt = m_connection.createStatement();
            stmt.execute(sSQL);
         }
         catch (SQLException e)
         {
            if (!m_bSafe)
            {
               throw new PersistenceException(
                  "err.persistence.executeSQL", new Object[]{ObjUtil.getMessage(e), sSQL}, e);
            }
         }
         finally
         {
            if (stmt != null)
            {
               try
               {
                  stmt.close();
               }
               catch (SQLException e)
               {
               }
            }
         }
      }
   }
}
