// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.SQLException;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLSubstReader;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.SchemaVersion;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * Schema manager for MySQL v5+.
 */
public class MySQLSchemaManager extends SQLSchemaManager
{
   /**
    * The default role to use for table permissions.
    */
   protected String m_sDefaultRole;

   // constructors

   /**
    * Constructs the schema manager.
    * @param adapter The persistence adapter.
    */
   public MySQLSchemaManager(MySQLAdapter adapter)
   {
      super(adapter);
      
      m_sDefaultRole = super.getDefaultRole();
   }

   // operations

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#analyzeTable(nexj.core.meta.persistence.sql.Table)
    */
   public void analyzeTable(Table table)
   {
      StringBuffer buf;

      if (table.getType() == Table.MANAGED)
      {
         buf = new StringBuffer(64);

         buf.append("analyze table ");
         appendTableName(buf, table);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnSuffix(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendColumnSuffix(StringBuffer buf, Column column)
   {
      if (column.isPrimary() && column.getTable().isIdentityKeyGenerator())
      {
         buf.append(" auto_increment");
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnType(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendColumnType(StringBuffer buf, Column column)
   {
      int nPrecision = column.getPrecision();
      
      switch (column.getType().getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            boolean bUnicode = ((RelationalDatabase)column.getTable().getSchema().getDataSource()).isUnicode();

            if (column.isLOB(MySQLAdapter.MAX_VARCHAR_PRECISION,
                             MySQLAdapter.MAX_VARBINARY_PRECISION))
            {
               buf.append("longtext");
            }
            else
            {
               buf.append((column.getAllocation() == Column.FIXED && // varchar if char too small
                           nPrecision <= MySQLAdapter.MAX_CHAR_PRECISION)
                          ? "char" : "varchar");
               buf.append('(');
               buf.append(nPrecision);
               buf.append(')');
            }

            buf.append(" character set ").append((bUnicode) ? "utf8" : "latin1");

            break;

         case Primitive.BINARY_ORDINAL:
            if (column.isLOB(MySQLAdapter.MAX_VARCHAR_PRECISION,
                             MySQLAdapter.MAX_VARBINARY_PRECISION))
            {
               buf.append("longblob");
            }
            else
            {
               buf.append((column.getAllocation() == Column.FIXED && // varbinary if binary too small
                           nPrecision <= MySQLAdapter.MAX_BINARY_PRECISION)
                          ? "binary" : "varbinary");
               buf.append('(');
               buf.append(nPrecision);
               buf.append(')');
            }

            break;
            
         case Primitive.INTEGER_ORDINAL:
            switch (nPrecision)
            {
               case 1:
                  buf.append("tinyint unsigned");
                  break;

               case 2:
                  buf.append("smallint");
                  break;

               case 3:
                  buf.append("mediumint");
                  break;

               default:
                  buf.append("int");
                  break;
            }

            break;
            
         case Primitive.LONG_ORDINAL:
            buf.append("bigint");
            break;
            
         case Primitive.DECIMAL_ORDINAL:
            buf.append("decimal(");
            buf.append(column.getPrecision(m_adapter.getMaxDecimalPrecision()));
            buf.append(',');
            buf.append(column.getScale(m_adapter.getMaxDecimalPrecision()));
            buf.append(')');

            break;
            
         case Primitive.FLOAT_ORDINAL:
            buf.append("float");
            break;

         case Primitive.DOUBLE_ORDINAL:
            buf.append("double");
            break;
            
         case Primitive.TIMESTAMP_ORDINAL:
            buf.append("datetime");
            break;
            
         case Primitive.BOOLEAN_ORDINAL:
            buf.append("boolean");
            break;
            
         default:
            throw new IllegalArgumentException("Invalid literal type: " + column.getType().getOrdinal() +
               " requested in appendColumnType(StringBuffer, Column) for column:" + column.getName());
      }

   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendConcatenate(java.lang.StringBuffer, java.lang.CharSequence[])
    */
   protected StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] array)
   {
      return (StringBuffer)StringUtil.join(buf, array, "concat(", ", ", ")");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendIndexColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendIndexColumn(StringBuffer buf, Column column)
   {
      super.appendIndexColumn(buf, column);

      // MySQL LOB columns require specifying comparison prefix during index creation
      // @see http://dev.mysql.com/doc/refman/5.0/en/create-index.html
      if (column.isLOB(MySQLAdapter.MAX_VARCHAR_PRECISION,
                       MySQLAdapter.MAX_VARBINARY_PRECISION))
      {
         int nLen = column.getPrecision();
         int nLenMax = 767; // MySQL has a limit of 767 bytes for LOB index columns

         if (column.getType() == Primitive.STRING &&
             ((RelationalDatabase)column.getTable().getSchema().getDataSource()).isUnicode())
         {
            nLenMax = 255; // MySQL assumes UTF8 == 3 bytes, hence Math.floor(nLenMax/3)
         }

         buf.append('(').append((nLen > 0 && nLen < nLenMax) ? nLen : nLenMax).append(')');
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendPrint(java.lang.StringBuffer, java.lang.String)
    */
   protected void appendPrint(StringBuffer buf, String msg)
   {
      buf.append("      select ");
      m_adapter.appendLiteral(buf, msg);
      buf.append(';');
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTableSuffix(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Table)
    */
   protected void appendTableSuffix(StringBuffer buf, Table table)
   {
      buf.append("engine=InnoDB"); // need to use InnoDB for ACID support/compliance

      if (((RelationalDatabase)(table.getSchema().getDataSource())).isUnicode())
      {
         buf.append(" character set = utf8");
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTSExtract(java.lang.StringBuffer, java.lang.CharSequence, byte)
    */
   protected StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField)
   {
      buf.append("extract(");
      appendTSField(buf, nField);
      buf.append(" from ");
      buf.append(sTS);
      buf.append(')');

      return buf;
   }

   /**
    * Append the SQL field name to buffer.
    * @param buf The destination buffer (not null).
    * @param nField One of SQLSubstReader.TS_* constants representing units of sDelta
    * @return The destination buffer.
    */
   protected StringBuffer appendTSField(StringBuffer buf, byte nField)
   {
      assert buf != null;

      switch (nField)
      {
         case SQLSubstReader.TS_YEAR:
            return buf.append("year");

         case SQLSubstReader.TS_QUARTER:
            return buf.append("quarter");

         case SQLSubstReader.TS_MONTH:
            return buf.append("month");

         case SQLSubstReader.TS_WEEK:
            return buf.append("week");

         case SQLSubstReader.TS_DAY:
            return buf.append("day");

         case SQLSubstReader.TS_HOUR:
            return buf.append("hour");

         case SQLSubstReader.TS_MIN:
            return buf.append("minute");

         case SQLSubstReader.TS_SEC:
            return buf.append("second");

         case SQLSubstReader.TS_USEC:
            return buf.append("microsecond");

         default:
            throw new IllegalArgumentException();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTSIncrement(java.lang.StringBuffer, java.lang.CharSequence, java.lang.CharSequence, byte)
    */
   protected StringBuffer appendTSIncrement(
      StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField)
   {
      buf.append("timestampadd(");
      appendTSField(buf, nField);
      buf.append(", ");
      buf.append(sDelta);
      buf.append(", ");
      buf.append(sTS);
      buf.append(')');

      return buf;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendUpgradeInitialize(java.lang.StringBuffer, nexj.core.meta.persistence.sql.RelationalSchema)
    */
   protected void appendUpgradeInitialize(StringBuffer buf, RelationalSchema schema)
   {
      // allow using doublequote when quoting column names in "CREATE TABLE" statements
      buf.append("set sql_mode = concat(@@sql_mode, ',ANSI_QUOTES')");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendUpgradeStepEnd(java.lang.StringBuffer, nexj.core.meta.persistence.sql.RelationalSchema, nexj.core.persistence.SchemaVersion, nexj.core.persistence.SchemaVersion, java.lang.String)
    */
   protected void appendUpgradeStepEnd(StringBuffer buf,
      RelationalSchema schema, SchemaVersion version, SchemaVersion prev, String sFailMsg)
   {
      if (version != null)
      {
         buf.append("      ");
         appendVersionTableUpdate(buf, schema, version, prev);
         buf.append(';');
         buf.append(SysUtil.LINE_SEP);
      }

      if (sFailMsg != null)
      {
         buf.append("   else");
         buf.append(SysUtil.LINE_SEP);
         appendPrint(buf, sFailMsg);
         buf.append(SysUtil.LINE_SEP);
      }

      buf.append("   end if;");
      buf.append(SysUtil.LINE_SEP);
      buf.append("end;");
      buf.append(SysUtil.LINE_SEP);
      buf.append("call _upgrade();");
      buf.append(SysUtil.LINE_SEP);
      buf.append("drop procedure _upgrade;");
      buf.append(SysUtil.LINE_SEP);
      buf.append('/');
      buf.append(SysUtil.LINE_SEP);
      buf.append("delimiter "); // SQLSchemaManager.appendSQL(String) will append ';' for the last row
   }

   /**
    * MySQL doesn't support conditional statements outside of stored procedures, so create a stored procedure for every step and link via @upgrade_error variable (to not execute remainder after first failure)
    * MySQL does not have a separate Unicode type, hence fail on misconfiguration.
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendUpgradeStepStart(java.lang.StringBuffer, nexj.core.meta.persistence.sql.RelationalSchema, nexj.core.persistence.SchemaVersion)
    * Note: Have to create a separate procedure for each step because MySQL will have trouble processing the packet size if all steps are in one long procedure.
    */
   protected void appendUpgradeStepStart(StringBuffer buf, RelationalSchema schema, SchemaVersion version)
   {
      RelationalDatabase ds = (RelationalDatabase)schema.getDataSource();

      buf.append("delimiter /");
      buf.append(SysUtil.LINE_SEP);
      buf.append("create procedure _upgrade()");
      buf.append(SysUtil.LINE_SEP);
      buf.append("begin");
      buf.append(SysUtil.LINE_SEP);

      // the table_schema column actually contains the database name
      buf.append("   if exists (select 1");
         appendVersionTableFrom(buf, schema, version);
      buf.append(") and exists(select character_set_name from information_schema.columns");
      buf.append(" where table_schema=");
      m_adapter.appendLiteral(
         buf, ((RelationalDatabaseFragment)ds.getDefaultFragment()).getDatabase());
      buf.append(" and table_name=");
      m_adapter.appendLiteral(buf, schema.getVersionTable().getTableName());
      buf.append(" and column_name=");
      m_adapter.appendLiteral(buf, "namespace");
      buf.append(" and character_set_name");
      buf.append((ds.isUnicode()) ? "" : " is not null and character_set_name!");
      buf.append('=');
      m_adapter.appendLiteral(buf, "utf8");
      buf.append(')');
      buf.append(SysUtil.LINE_SEP);
      buf.append("   then");
      buf.append(SysUtil.LINE_SEP);

      if (version.getStep() >= 0)
      {
         appendPrint(buf, "Upgrade version \"" + version.getVersion() + "\", step " + version.getStep());
         buf.append(SysUtil.LINE_SEP);
      }
   }

   /**
    * Create the Full-Text counterpart table to the requested table.
    * @param table The table to create the Full-Text counterpart for.
    */
   protected void createTextTable(Table table)
   {
      StringBuffer columns = new StringBuffer(64);//used for populating Full-Text table(pk,a,b,...)
      Index primaryKey = table.getPrimaryKey();
      StringBuffer buf = new StringBuffer(128);

      buf.append("create table ");
      appendTableName(buf, table, "$");
      buf.append('(');

      for (int i = 0, nCount = primaryKey.getIndexColumnCount(); i < nCount; ++i) // add PK cols
      {
         Column col = table.getColumn(i);

         appendColumnDeclaration(buf, col, true, true, ", ");
         appendColumnName(columns, col);
         buf.append(','); // last one will be followed by PK declaration at end
         columns.append(','); // last one will be truncated later
      }

      appendPrimaryKey(buf, primaryKey);
      buf.append(") engine=MyISAM"); // need to use MyISAM for Full-Text indexes
      columns.setLength(columns.length() - 1); // truncate last ','

      if (((RelationalDatabase)(table.getSchema().getDataSource())).isUnicode())
      {
         buf.append(" character set = utf8");
      }

      m_appender.appendSQL(buf.toString());
      buf.setLength(0);
      createTextTriggers(table, primaryKey, null);
      buf.append("insert into ");
      appendTableName(buf, table, "$");
      buf.append('(').append(columns).append(") select ");
      buf.append(columns).append(" from ");
      appendTableName(buf, table);
      m_appender.appendSQL(buf.toString()); // fill Full-Text table with data from source table
      buf.setLength(0);
   }

   /**
    * Create Full-Text triggers used for updating Full-Text table from requested table.
    * The indexes considered are the PrimaryKey index and all Full-Text indexes.
    * If includeIndex == null && excludeIndex == null
    *    => use all indexes from table.
    * If includeIndex == null && excludeIndex != null
    *    => use all indexes except excludeIndex.
    * If includeIndex != null && excludeIndex == null
    *    => use only includeIndex.
    * If includeIndex != null && excludeIndex != null
    *    => use includeIndex and all indexes except excludeIndex.
    * @param table The table to create the triggers for.
    * @param includeIndex The index to include.
    * @param excludeIndex The index to omit.
    */
   protected void createTextTriggers(Table table, Index includeIndex, Index excludeIndex)
   {
      Index primaryKey = table.getPrimaryKey();

      if (primaryKey == excludeIndex)
      {
         return; // invalid trigger action
      }

      StringBuffer changedColumns = new StringBuffer(64); // used for trigger SQL (new.pk,new.a,)
      StringBuffer listingColumns = new StringBuffer(64); // used for trigger SQL (pk,a,)
      StringBuffer matchedColumns = new StringBuffer(64); // used for trigger SQL (pk=old.pk and)
      StringBuffer updatedColumns = new StringBuffer(64); // used for trigger SQL (a=new.a,)

      // determine column listings transfered to Full-Text table
      for (int i = 0, nCount = table.getIndexCount(); i < nCount; ++i)
      {
         Index idx = table.getIndex(i);

         if (idx.getType() == Index.TEXT && idx != excludeIndex &&
             (includeIndex == null || idx == includeIndex))
         {
            Column col = idx.getIndexColumn(0).getColumn();

            if (primaryKey.findIndexColumn(col) == null) // add PK columns later
            {
               changedColumns.append("new.");
               appendColumnName(changedColumns, col);
               appendColumnName(listingColumns, col);
               appendColumnName(updatedColumns, col);
               updatedColumns.append(" = new.");
               appendColumnName(updatedColumns, col);
               changedColumns.append(','); // followed by PrimaryKey columns
               listingColumns.append(','); // followed by PrimaryKey columns
               updatedColumns.append(','); // followed by PrimaryKey columns
            }
         }
      }

      for (int i = 0, nCount = primaryKey.getIndexColumnCount(); i < nCount; ++i) // add PK cols
      {
         if (i != 0)
         {
            changedColumns.append(',');
            listingColumns.append(',');
            matchedColumns.append(" and ");
            updatedColumns.append(',');
         }

         Column col = primaryKey.getIndexColumn(i).getColumn();

         changedColumns.append("new.");
         appendColumnName(changedColumns, col);
         appendColumnName(listingColumns, col);
         appendColumnName(matchedColumns, col);
         matchedColumns.append(" = old.");
         appendColumnName(matchedColumns, col);
         appendColumnName(updatedColumns, col);
         updatedColumns.append(" = new.");
         appendColumnName(updatedColumns, col);
      }

      StringBuffer buf = new StringBuffer(64);

      buf.append("create trigger ");
      appendTableName(buf, table, "$d");
      buf.append(" after delete on ");
      appendTableName(buf, table);
      buf.append(" for each row delete from ");
      appendTableName(buf, table, "$");
      buf.append(" where ").append(matchedColumns);
      m_appender.appendSQL(buf.toString());
      buf.setLength(0);
      buf.append("create trigger ");
      appendTableName(buf, table, "$i");
      buf.append(" after insert on ");
      appendTableName(buf, table);
      buf.append(" for each row insert into ");
      appendTableName(buf, table, "$");
      buf.append('(').append(listingColumns).append(") values (");
      buf.append(changedColumns).append(')');
      m_appender.appendSQL(buf.toString());
      buf.setLength(0);
      buf.append("create trigger ");
      appendTableName(buf, table, "$u");
      buf.append(" after update on ");
      appendTableName(buf, table);
      buf.append(" for each row update ");
      appendTableName(buf, table, "$");
      buf.append(" set ").append(updatedColumns);
      buf.append(" where ").append(matchedColumns);
      m_appender.appendSQL(buf.toString());
   }

   /**
    * MySQL only supports Full-Text Search on MyISAM tables, but adapter uses InnoDB tables for
    * ACID compliance. Hence create a second table (MyISAM) having only columns required for
    * Full-Text indexes. Set up delete/insert/update triggers and copy over the data from source
    * table. Then create Full-Text indexes on the new table.
    * @see nexj.core.persistence.sql.SQLSchemaManager#createIndex(nexj.core.meta.persistence.sql.Index)
    */
   protected void createIndex(Index index)
   {
      Table table = index.getTable();

      if (index.getType() != Index.TEXT || table.getType() != Table.MANAGED)
      {
         super.createIndex(index); // let super deal with regular indexes

         return;
      }

      Column column = index.getIndexColumn(0).getColumn();
      Index primaryKey = table.getPrimaryKey();
      boolean bColInPK = primaryKey.findIndexColumn(column) != null;
      boolean bHaveMoreIndexes = false;
      StringBuffer buf = new StringBuffer(128);

      // determine if this is the first Full-Text index to be created (i.e. Full-Text table needed)
      for (int i = 0, nCount = table.getIndexCount(); i < nCount && !bHaveMoreIndexes; ++i)
      {
         Index idx = table.getIndex(i);

         bHaveMoreIndexes = idx != index && idx.getType() == Index.TEXT;
      }

      if (!bHaveMoreIndexes) // Need to create a new Full-Text table
      {
         createTextTable(table);
      }

      if (!bColInPK) // Existing Full-Text table to alter, column does not exist yet
      {
         buf.append("alter table ");
         appendTableName(buf, table, "$");
         buf.append(" add column ");
         appendColumnDeclaration(buf, column, false, false, ", "); // take only first column
         m_appender.appendSQL(buf.toString());           // column nullable since rows can exist
         buf.setLength(0);
         dropTextTriggers(table);
         createTextTriggers(table, null, null);
         buf.append("update ");
         appendTableName(buf, table, "$");
         buf.append(" dst inner join ");
         appendTableName(buf, table);
         buf.append(" src on ");

         for (int i = 0, nCount = primaryKey.getIndexColumnCount(); i < nCount; ++i) // add PK cols
         {
            Column col = table.getColumn(i);

            if (i != 0)
            {
               buf.append("and ");
            }

            buf.append("dst.");
            appendColumnName(buf, col);
            buf.append(" = src.");
            appendColumnName(buf, col);
         }

         buf.append(" set dst.");
         appendColumnName(buf, column);
         buf.append(" = src.");
         appendColumnName(buf, column);
         m_appender.appendSQL(buf.toString()); // fill Full-Text column with data from source table
         buf.setLength(0);
      }

      buf.append("create fulltext index ").append(getIndexName(index, false, true));
      buf.append(" on ");
      appendTableName(buf, table, "$");
      buf.append('(');
      appendIndexColumn(buf, column);
      buf.append(')');
      m_appender.appendSQL(buf.toString()); // create Full-Text index on Full-Text column
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#createIndexes(nexj.core.meta.persistence.sql.Table)
    */
   protected void createIndexes(Table table)
   {
      boolean bHasManyTextIndexes = false;

      // determine if this is the first Full-Text index to be created (i.e. Full-Text table needed)
      for (int i = 0, j = 0, nCount = table.getIndexCount();
           i < nCount && !bHasManyTextIndexes;
           ++i)
      {
         j += (table.getIndex(i).getType() == Index.TEXT) ? 1 : 0;
         bHasManyTextIndexes = j > 1;
      }

      if (bHasManyTextIndexes) // if only 1 index then createIndex() will create Full-Text table
      {
         createTextTable(table); // ensure Full-Text table present when indexes being created
      }

      super.createIndexes(table);
   }

   /**
    * Drop Full-Text triggers used for updating Full-Text table from requested table.
    * @param table The table to drop the triggers from.
    */
   protected void dropTextTriggers(Table table)
   {
      StringBuffer buf = new StringBuffer(64);

      buf.append("drop trigger if exists ");
      appendTableName(buf, table, "$d");
      m_appender.appendSQL(buf.toString()); // drop "delete" trigger
      buf.setLength(0);
      buf.append("drop trigger if exists ");
      appendTableName(buf, table, "$i");
      m_appender.appendSQL(buf.toString()); // drop "insert" trigger
      buf.setLength(0);
      buf.append("drop trigger if exists ");
      appendTableName(buf, table, "$u");
      m_appender.appendSQL(buf.toString()); // drop "update" trigger
   }

   /**
    * MySQL treats drop of Primary Key differently,
    * also MySQL indexes explicitly belong to a specific table.
    * @see nexj.core.persistence.sql.SQLSchemaManager#dropIndex(nexj.core.meta.persistance.sql.Index)
    */
   protected void dropIndex(Index index)
   {
      Table table = index.getTable();
      
      if (index.getType() >= Index.BTREE && table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(128);

         if (isConstraint(index))
         {
            buf.append("alter table ");
            appendTableName(buf, table);
            buf.append(" drop primary key");
         }
         else
         {
            buf.append("drop index ");
            buf.append(getFullIndexName(index, true));
            buf.append(" on ");
            appendTableName(buf, table);
         }

         m_appender.appendSQL(buf.toString());
      }
      else if (index.getType() == Index.TEXT && table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(128);

         buf.append("drop index ");
         buf.append(getFullIndexName(index, true));
         buf.append(" on ");
         appendTableName(buf, table, "$");
         m_appender.appendSQL(buf.toString());
         buf.setLength(0);

         boolean bHaveMoreIndexes = false;
         Column column = index.getIndexColumn(0).getColumn(); // take only first column

         // see if if there are any more FullText indexes
         for (int i = 0, nCount = table.getIndexCount(); i < nCount && !bHaveMoreIndexes; ++i)
         {
            Index idx = table.getIndex(i);

            bHaveMoreIndexes = idx != index && idx.getType() == Index.TEXT; // ignore self
         }

         // the Full-Text column is part of PrimaryKey, so it is still needed for other Full-Text
         if (bHaveMoreIndexes &&                                                       // indexes
             table.getPrimaryKey().findIndexColumn(column) != null)
         {
            return; // done
         }

         dropTextTriggers(table);

         if (bHaveMoreIndexes) // Existing Full-Text table to alter
         {
            createTextTriggers(table, null, index);
            buf.append("alter table ");
            appendTableName(buf, table, "$");
            buf.append(" drop column ");
            appendColumnName(buf, column);
         }
         else // no more need for the Full-Text table
         {
            buf.append("drop table ");
            appendTableName(buf, table, "$");
         }

         m_appender.appendSQL(buf.toString());
      }
      else
      {
         super.dropIndex(index);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#dropTable(nexj.core.meta.persistence.sql.Table, boolean)
    */
   public void dropTable(Table table, boolean bAll)
   {
      super.dropTable(table, bAll);

      // drop Full-Text table as well
      if (table.getType() == Table.MANAGED && hasTextTable(table))
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("drop table ");
         appendTableName(buf, table, "$");

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getAlterColumnToken()
    */
   protected String getAlterColumnToken()
   {
      return " modify ";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getCreateEtcScriptName()
    */
   protected String getCreateEtcScriptName()
   {
      return "mysql_create.sql";
   }

   /**
    * there is no notion of tablespaces for indexes in MySQL
    *
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultIndexspace()
    */
   protected String getDefaultIndexspace()
   {
      return null;
   }

   /**
    * there is no notion of tablespaces for columns in MySQL
    *
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultLongspace()
    */
   protected String getDefaultLongspace()
   {
      return null;
   }

   /**
    * there is no notion of tablespaces in MySQL (the closest is DATA DIRECTORY/INDEX DIRECTORY see http://dev.mysql.com/doc/refman/5.0/en/create-table.html)
    *
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultTablespace()
    */
   protected String getDefaultTablespace()
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultRole()
    */
   public String getDefaultRole()
   {
      return m_sDefaultRole;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDropSuccessor(nexj.core.meta.persistence.sql.Table, nexj.core.meta.persistence.sql.Table)
    */
   protected Table getDropSuccessor(Table left, Table right)
   {
      return (hasTextTable(left) || !hasTextTable(right)) ? left : right;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDropEtcScriptName()
    */
   protected String getDropEtcScriptName()
   {
      return null; // nothing to drop
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDynamicSQLAppender(java.lang.StringBuffer)
    */
   protected SQLAppender getDynamicSQLAppender(final StringBuffer buf)
   {
      return new SQLAppender()
      {
         public void appendSQL(String sSQL)
         {
            buf.append("      ");
            buf.append(sSQL);
            buf.append(';');
            buf.append(SysUtil.LINE_SEP);
         }
      };
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getFullTableName(java.lang.String, java.lang.String)
    */
   protected String getFullTableName(String sSchemaName, String sTableName)
   {
      // MySQL does not support the concept of schema, only of catalog.
      // MySQL does not return schema names for tables, rather it returns the catalog name. Since
      // the only fn to use this is readSchema() kludge this by using connection's catalog name.
      if (sSchemaName == null && m_connection != null)
      {
         try
         {
            sSchemaName = m_connection.getCatalog();
         }
         catch (SQLException e) // failed to get catalog name
         {
         }
      }

      // MySQL returns table names in lower case in win32, the only user of this fn is readSchema()
      return super.getFullTableName(sSchemaName, sTableName);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getGUIDExpr()
    */
   protected String getGUIDExpr()
   {
      return "uuid()";
   }

   /**
    * Gets an index name. (override parent because MySQL doesn't understand index names prefixed with table name and there's no need for table prefix)
    * @param index The index object.
    * @param bConstraint True if the name is for a constraint clause.
    * @param bQuote True to quote the keywords.
    * @return The full index name.
    */
   protected String getIndexName(Index index, boolean bConstraint, boolean bQuote)
   {
      String sTableName = index.getTable().getTableName();
      String sIndexName = index.getName();

      // remove table name prefix from the index name since MySQL indexes are unique per table
      if (sIndexName.length() > sTableName.length() + 1 &&
          sIndexName.startsWith(sTableName) && sIndexName.charAt(sTableName.length()) == '.')
      {
         sIndexName = sIndexName.substring(sTableName.length() + 1);
      }

      return getIndexName(sIndexName, "", "", bConstraint, bQuote);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getNowExpr()
    */
   protected String getNowExpr()
   {
      return "now()";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getSeparator()
    */
   public String getSeparator()
   {
      return ';' + SysUtil.LINE_SEP;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getSetupEtcScriptName()
    */
   protected String getSetupEtcScriptName()
   {
      return "mysql_setup.sql";
   }

   /**
    * Determine if supplied table has an Full-Text table counterpart.
    * @param table The table to check.
    * @return The requested table has an Full-Text table counterpart.
    */
   protected static boolean hasTextTable(Table table)
   {
      for (int i = 0, nCount = table.getIndexCount(); i < nCount; ++i)
      {
         if (table.getIndex(i).getType() == Index.TEXT)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#isImplicitConversion(nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column)
    */
   protected boolean isImplicitConversion(Column source, Column target)
   {
      Primitive srcType = source.getType();
      Primitive dstType = target.getType();

      if (srcType == dstType)
      {
         return true; // same type
      }

      if (dstType == Primitive.BOOLEAN)
      {
         return false; // must explicitly compare != 0
      }

      if (srcType == Primitive.TIMESTAMP &&
          (dstType == Primitive.DOUBLE || dstType == Primitive.FLOAT ||
           dstType == Primitive.INTEGER || dstType == Primitive.LONG))
      {
         return false; // must interpret as milliseconds
      }

      if (dstType == Primitive.TIMESTAMP &&
          (srcType == Primitive.DECIMAL ||
           srcType == Primitive.DOUBLE || srcType == Primitive.FLOAT ||
           srcType == Primitive.INTEGER || srcType == Primitive.LONG))
      {
         return false; // must interpret as milliseconds
      }

      if ((dstType == Primitive.BINARY && srcType == Primitive.STRING) ||
          (dstType == Primitive.STRING && srcType == Primitive.BINARY))
      {
         return false; // requires explicit conversion
      }

      return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#renameColumn(nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column)
    */
   protected void renameColumn(Column newColumn, Column oldColumn)
   {
      StringBuffer buf;
      Table table = oldColumn.getTable();

      if (table.getType() != Table.MANAGED)
      {
         return;
      }

      buf = new StringBuffer(128);
      buf.append("alter table ");
      appendTableName(buf, table);
      buf.append(" change column ");
      appendColumnName(buf, oldColumn);
      buf.append(" ");
      appendColumnDeclaration(buf, newColumn, null, oldColumn.isNullable(), false);

      m_appender.appendSQL(buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#renameTable(nexj.core.meta.persistence.sql.Table, nexj.core.meta.persistence.sql.Table)
    */
   protected void renameTable(Table newTable, Table oldTable)
   {
      if (oldTable.getType() != Table.MANAGED)
      {
         super.renameTable(newTable, oldTable);

         return;
      }

      StringBuffer buf = new StringBuffer(64);

      buf.append("alter table ");
      appendTableName(buf, oldTable);
      buf.append(" rename ");
      appendTableName(buf, newTable);
      m_appender.appendSQL(buf.toString());

      if (hasTextTable(oldTable))
      {
         buf.setLength(0);
         dropTextTriggers(oldTable); // table used only to get old name, which is what is needed
         buf.append("alter table ");
         appendTableName(buf, oldTable, "$");
         buf.append(" rename ");
         appendTableName(buf, newTable, "$");
         m_appender.appendSQL(buf.toString()); // rename the Full-Text table as well
         createTextTriggers(newTable, null, null);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#setFragment(nexj.core.meta.persistence.sql.RelationalDatabaseFragment)
    */
   public void setFragment(RelationalDatabaseFragment fragment)
   {
      super.setFragment(fragment);

      // MySQL doesn't have a concept of roles/groups so have to map it to the user login
      m_sDefaultRole = fragment.getUser();
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#truncateTable(nexj.core.meta.persistence.sql.Table)
    */
   public void truncateTable(Table table)
   {
      super.truncateTable(table);

      if (table.getType() == Table.MANAGED && hasTextTable(table))
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("truncate table ");
         appendTableName(buf, table, "$");
         m_appender.appendSQL(buf.toString()); // truncate the Full-Text table as well
      }
   }
}