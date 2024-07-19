// Copyright 2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.Types;
import java.util.Locale;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;
import nexj.core.meta.persistence.sql.RelationalDatabase;
import nexj.core.meta.persistence.sql.RelationalDatabaseFragment;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.SQLSubstReader;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.SchemaVersion;
import nexj.core.util.PropertyMap;
import nexj.core.util.StringUtil;
import nexj.core.util.SysUtil;

/**
 * This manager supports PostgreSQL versions 8.4, 9.0.
 * On Windows, for x64 bit versions, uuid-ossp.sql is not available with the installer
 * Unzip http://winpg.jp/~saito/pg_work/OSSP_win32/build-x86-64/pg90_uuid_ossp_x64.zip into the installation directory
 * before installing uuid-ossp.sql
 */
public class PostgreSQLSchemaManager extends CaseInsensitiveSQLSchemaManager
{
   // attributes

   /**
    * True if the DB product is a restricted version for a cloud.
    */
   protected boolean m_bClouded;

   // associations

   /**
    * The RelationSchema to manage
    */
   protected RelationalSchema m_schema;

   // constructor

   /**
    * @see SQLSchemaManager#init(RelationalSchema)
    */
   public PostgreSQLSchemaManager(PostgreSQLAdapter adapter)
   {
      super(adapter);
      m_bClouded = adapter.isClouded();
   }

   // operations

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getTablespace(nexj.core.meta.persistence.sql.Table)
    */
   protected String getTablespace(Table table)
   {
      return (m_bClouded) ? null : super.getTablespace(table);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getIndexspace(nexj.core.meta.persistence.sql.Index)
    */
   protected String getIndexspace(Index index)
   {
      return (m_bClouded) ? null : super.getIndexspace(index);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#analyzeTable(nexj.core.meta.persistence.sql.Table)
    */
   public void analyzeTable(Table table)
   {
      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(128);

         buf.append("vacuum analyze ");
         appendTableName(buf, table);

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnAlteration(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column, java.lang.String)
    */
   protected void appendColumnAlteration(StringBuffer buf, Column newColumn, Column oldColumn, String sSuffix)
   {
      boolean bChangedType = false;
      boolean bChangedPrecision = false;

      if (oldColumn.getType() != newColumn.getType())
      {
         appendColumnName(buf, oldColumn, sSuffix);

         int nOldType = oldColumn.getType().getOrdinal();
         int nNewType = newColumn.getType().getOrdinal();

         buf.append(" type ");
         appendColumnType(buf, newColumn);
         buf.append(" using ");

         if (nNewType == Primitive.TIMESTAMP_ORDINAL &&
            (nOldType >= Primitive.INTEGER_ORDINAL && nOldType <= Primitive.DOUBLE_ORDINAL))
         {
            buf.append("to_timestamp(");
            appendColumnName(buf, oldColumn);
            buf.append(')');
         }
         else if (nOldType == Primitive.TIMESTAMP_ORDINAL &&
                 (nNewType >= Primitive.INTEGER_ORDINAL && nNewType <= Primitive.DOUBLE_ORDINAL))
         {
            buf.append("extract(epoch from ");
            appendColumnName(buf, oldColumn);
            buf.append(')');
         }
         else
         {
            appendColumnName(buf, oldColumn);
            buf.append("::");
            appendColumnType(buf, newColumn);
         }

         bChangedType = true;
      }
      else if (oldColumn.getPrecision() != newColumn.getPrecision())
      {
         appendColumnName(buf, oldColumn, sSuffix);
         buf.append(" type ");
         appendColumnType(buf, newColumn);
         bChangedPrecision = true;
      }

      if (oldColumn.isNullable() != newColumn.isNullable())
      {
         if (bChangedPrecision || bChangedType)
         {
            buf.append(getAlterColumnSeparator());
            buf.append(getAlterColumnToken());
         }

         appendColumnName(buf, oldColumn, sSuffix);
         buf.append(newColumn.isNullable() ? " drop " : " set ");
         buf.append("not null");
      }
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#appendColumnCaseConversion(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendColumnCaseConversion(StringBuffer buf, Column column)
   {
      buf.append("upper(");
      appendColumnName(buf, column);
      buf.append(')');
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#appendColumnDeclaration(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, boolean, boolean, java.lang.String)
    */
   protected void appendColumnDeclaration(StringBuffer buf, Column column, boolean bNullability, boolean bCreateTable, String sSep)
   {
      super.appendColumnDeclaration(buf, column,bNullability, bCreateTable, bCreateTable ? sSep : ", " + sSep);
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#appendColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column, java.lang.String)
    */
   protected void appendColumn(StringBuffer buf, Column column, Column target, String sSep)
   {
      super.appendColumn(buf, column, target, ", ");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnType(java.lang.StringBuffer,nexj.core.meta.persistence.sql.Column)
    */
   protected void appendColumnType(StringBuffer buf, Column column)
   {
      int nPrecision = column.getPrecision();

      switch (column.getType().getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            if (column.isLOB(PostgreSQLAdapter.MAX_VARCHAR_PRECISION,0))
            {
               buf.append("text");
            }
            else
            {
               buf.append(column.getAllocation() == Column.FIXED ? "char" : "varchar");

               if (nPrecision > 0)
               {
                  buf.append('(');
                  buf.append(nPrecision);
                  buf.append(')');
               }
            }

            break;

         case Primitive.INTEGER_ORDINAL:
            if (column.isPrimary() && column.getTable().isIdentityKeyGenerator())
            {
               //Maximum value of 2147483647, signed 4 byte storage
               buf.append("serial");
            }
            else
            {
               buf.append((nPrecision == 1 || nPrecision == 2) ? "smallint" : "integer");
            }

            break;

         case Primitive.LONG_ORDINAL:
            if (column.isPrimary() && column.getTable().isIdentityKeyGenerator())
            {
               // Maximum value of 9223372036854775807, signed 8 byte storage
               buf.append("bigserial");
            }
            else
            {
               buf.append("bigint");
            }

            break;

         case Primitive.FLOAT_ORDINAL:
            buf.append("real");

            break;

         case Primitive.DOUBLE_ORDINAL:
            buf.append("double precision");

            break;

         case Primitive.TIMESTAMP_ORDINAL:
            buf.append("timestamp with time zone");

            break;

         case Primitive.BOOLEAN_ORDINAL:
            buf.append("boolean");

            break;

         case Primitive.DECIMAL_ORDINAL:
            buf.append("numeric(");
            buf.append(column.getPrecision(m_adapter.getMaxDecimalPrecision()));
            buf.append(',');
            buf.append(column.getScale(m_adapter.getMaxDecimalPrecision()));
            buf.append(')');

            break;

         case Primitive.BINARY_ORDINAL:
            if (isBLOBColumn(column))
            {
               // Instead of using oid type, use the lo type which wraps oid
               // Reference http://www.postgresql.org/docs/9.0/static/lo.html
               buf.append("lo");
            }
            else
            {
               buf.append("bytea"); // Max 1GB
            }

            break;

         default:
            throw new IllegalArgumentException("Invalid literal type: " + column.getType().getOrdinal()
                     + " requested in appendColumnType(StringBuffer, Column) for column:" + column.getName());
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendConcatenate(java.lang.StringBuffer, java.lang.CharSequence[])
    */
   protected StringBuffer appendConcatenate(StringBuffer buf, CharSequence[] argArray)
   {
      return (StringBuffer)StringUtil.join(buf, argArray, "(", " || ", ")");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendIndexColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.IndexColumn)
    */
   protected void appendIndexColumn(StringBuffer buf, IndexColumn indexColumn)
   {
      appendIndexColumn(buf, indexColumn.getColumn());

      //PostgreSQL does not support PRIMARY KEY(column [asc|desc])
      if (!isPrimaryKey(indexColumn.getIndex()) && !indexColumn.isAscending())
      {
         buf.append(" desc");
      }
   }

   /**
    * Appends a fill factor for a given index to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    */
   protected void appendIndexFillFactor(StringBuffer buf, Index index)
   {
      int nIndexFill = index.getFill();

      if (nIndexFill == -1)
      {
         nIndexFill = index.getTable().getSchema().getIndexFill(); // use default fill value for schema

         if (nIndexFill == 0 )
         {
            return ; // use database default fill factor
         }
      }

      //B-trees use a default fillfactor of 90, but any integer value from 10 to 100 can be selected.
      //reference http://www.postgresql.org/docs/8.4/static/sql-createindex.html
      buf.append(" with (fillfactor = ").append(Math.max(10, nIndexFill)).append(')');
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendIndexSuffix(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Index)
    */
   protected void appendIndexSuffix(StringBuffer buf, Index index)
   {
      appendIndexSuffix(buf, index, null);
   }

   /**
    * Appends the suffix for creating an index (create index on obj() ...) to a string buffer.
    * @param buf The destination string buffer.
    * @param index The index object.
    * @param sTablespaceCreatePrefix A prefix for create tablespace statement.
    */
   protected void appendIndexSuffix(StringBuffer buf, Index index, String sTablespaceCreatePrefix)
   {
      appendIndexFillFactor(buf, index);

      String sIndexspace = getIndexspace(index);

      if (!StringUtil.isEmpty(sIndexspace))
      {
         if (sTablespaceCreatePrefix != null)
         {
            buf.append(sTablespaceCreatePrefix);
         }

         buf.append(" tablespace ").append(sIndexspace);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendPrimaryKeySuffix(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Index)
    */
   protected void appendPrimaryKeySuffix(StringBuffer buf, Index index)
   {
     appendIndexSuffix(buf, index, " using index");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendPrint(java.lang.StringBuffer, java.lang.String)
    */
   protected void appendPrint(StringBuffer buf, String sMsg)
   {
      buf.append("select ");
      m_adapter.appendLiteral(buf, sMsg);
      buf.append(';');
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTableSuffix(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Table)
    */
   protected void appendTableSuffix(StringBuffer buf, Table table)
   {
      String sTablespace = getTablespace(table);

      if (!StringUtil.isEmpty(sTablespace))
      {
         buf.append(" tablespace ").append(sTablespace);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTSExtract(java.lang.StringBuffer, java.lang.CharSequence,byte)
    */
   protected StringBuffer appendTSExtract(StringBuffer buf, CharSequence sTS, byte nField)
   {
      buf.append("extract(");
      appendTSField(buf, nField);
      buf.append(" from ");
      buf.append(sTS);
      buf.append(")");

      return buf;
   }

   /**
    * Append the SQL field name to buffer.
    *
    * @param buf The destination buffer (not null).
    * @param nField One of SQLSubstReader.TS_* constants representing units of sDelta.
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
            return buf.append("microseconds");

         default:
            throw new IllegalArgumentException();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendTSIncrement(java.lang.StringBuffer, java.lang.CharSequence,java.lang.CharSequence, byte)
    */
   protected StringBuffer appendTSIncrement(StringBuffer buf, CharSequence sTS, CharSequence sDelta, byte nField)
   {
      buf.append(sTS);
      buf.append(" + ");

      if (nField == SQLSubstReader.TS_QUARTER)
      {
         // postgresql does not support interval'# quarter' instead use arithmetic * 3 months
         buf.append('(');
         buf.append(sDelta);
         buf.append(" * interval '3 ");
         appendTSField(buf, SQLSubstReader.TS_MONTH);
         buf.append("')");
      }
      else
      {
         buf.append("interval '");
         buf.append(sDelta);
         buf.append(' ');
         appendTSField(buf, nField);
         buf.append('\'');
      }

      return buf;
   }

   /**
    * Appends to a buffer, SQL to update a column rows to null.
    * @param buf The buffer to which to append SQL.
    * @param column The column that is to be set to null.
    */
   protected void appendUpdateColumnRowsToNull(StringBuffer buf, Column column)
   {
      buf.append("update ");
      appendTableName(buf, column.getTable());
      buf.append(" set ");
      appendColumnName(buf, column);
      buf.append(" = null");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendUpgradeStepEnd(java.lang.StringBuffer, nexj.core.meta.persistence.sql.RelationalSchema, nexj.core.persistence.SchemaVersion, nexj.core.persistence.SchemaVersion, java.lang.String)
    */
   protected void appendUpgradeStepEnd(StringBuffer buf, RelationalSchema schema, SchemaVersion version, SchemaVersion prev, String sFailMsg)
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
      buf.append("$f$ language plpgsql;");
      buf.append(SysUtil.LINE_SEP);
      buf.append("select * from _upgrade();");
      buf.append(SysUtil.LINE_SEP);
      buf.append("drop function _upgrade();");
      buf.append(SysUtil.LINE_SEP);
      buf.append("commit");
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendUpgradeStepStart(java.lang.StringBuffer, nexj.core.meta.persistence.sql.RelationalSchema, nexj.core.persistence.SchemaVersion)
    */
   protected void appendUpgradeStepStart(StringBuffer buf, RelationalSchema schema, SchemaVersion version)
   {
      RelationalDatabase ds = (RelationalDatabase)schema.getDataSource();

      buf.append(SysUtil.LINE_SEP);
      buf.append("begin;");
      buf.append(SysUtil.LINE_SEP);
      buf.append("create function _upgrade() returns void AS $f$");
      buf.append(SysUtil.LINE_SEP);
      buf.append("begin");
      buf.append(SysUtil.LINE_SEP);
      buf.append("   if exists (select true");
      appendVersionTableFrom(buf, schema, version);
      buf.append(") and exists (select encoding from pg_database where datname = ");
      m_adapter.appendLiteral(buf, ((RelationalDatabaseFragment)ds.getDefaultFragment()).getDatabase());
      buf.append(" and pg_encoding_to_char(encoding)");
      buf.append((ds.isUnicode()) ? " = " : " != ");
      buf.append("'UTF8'");
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
    * For a full-text search index, creates a trigger to update a tsvector column for the index's column.
    * @param index The full-text search index on whose column a trigger will be created.
    */
   protected void createFullTextSearchTrigger(Index index)
   {
      StringBuffer buf = new StringBuffer(256);
      Table table = index.getTable();
      Column column = index.getIndexColumn(0).getColumn();

      // NOTE http://www.postgresql.org/docs/9.0/static/textsearch-features.html#TEXTSEARCH-UPDATE-TRIGGERS
      // PostgreSQL provides built-in function tsvector_update_trigger
      // This is limited because you have to set the text_search_configuration at trigger creation time

      // create trigger procedure
      buf.append("create function ");
      appendFullTextProcedureName(buf, table, index, true);
      buf.append("() returns trigger as $f$ begin new.");
      buf.append(column.getName());
      buf.append("_ := to_tsvector(new.");
      appendColumnName(buf, column);
      buf.append("); return new; end $f$ language plpgsql");
      m_appender.appendSQL(buf.toString());

      buf.setLength(0);
      buf.append("create trigger ");
      buf.append(column.getName());
      buf.append("_ before insert or update on ");
      appendTableName(buf, table);
      buf.append(" for each row execute procedure ");
      appendFullTextProcedureName(buf, table, index, true);
      buf.append("()");

      m_appender.appendSQL(buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#createIndex(nexj.core.meta.persistence.sql.Index)
    */
   protected void createIndex(Index index)
   {
      Table table = index.getTable();

      if (table.getType() != Table.MANAGED || !isFullTextSearchIndex(index))
      {
         if (!m_bClouded)
         {
            super.createIndex(index);
         }

         return;
      }

      StringBuffer buf = new StringBuffer(256);
      Column column = index.getIndexColumn(0).getColumn();

      // add new column IndexColumn_
      buf.append("alter table ");
      appendTableName(buf, table);
      buf.append(" add column ");
      buf.append(column.getName());
      buf.append("_ tsvector");
      m_appender.appendSQL(buf.toString());
      buf.setLength(0);

      // copy data from existing column to the new one
      buf.append("update ");
      appendTableName(buf, table);
      buf.append(" set ");
      buf.append(column.getName());
      buf.append("_ = tsvector(");
      appendColumnName(buf, column);
      buf.append(')');
      m_appender.appendSQL(buf.toString());
      buf.setLength(0);

      // create index
      buf.append("create index ");
      buf.append(getIndexName(index, false, true));
      buf.append(" on ");
      appendTableName(buf, table);
      buf.append(" using gin(");
      buf.append(column.getName());
      buf.append("_)"); //gin uses FASTUPDATE parameter instead of FILLFACTOR and default is ON
      m_appender.appendSQL(buf.toString());

      createFullTextSearchTrigger(index);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#createTable(nexj.core.meta.persistence.sql.Table)
    */
   public void createTable(Table table)
   {
      super.createTable(table);

      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(256);
         String sDescription = table.getDescription();

         if (sDescription != null) //Add table comment
         {
            buf.append("comment on table ");
            appendTableName(buf, table);
            buf.append(" is ");
            m_adapter.appendLiteral(buf, sDescription);
            m_appender.appendSQL(buf.toString());
         }

         for (int i = table.getColumnCount() - 1; i > -1 ; --i)
         {
            Column column = table.getColumn(i);
            sDescription = column.getDescription();

            //Add column comments
            if (sDescription != null)
            {
               buf.setLength(0);
               buf.append("comment on column ");
               appendTableName(buf, table);
               buf.append('.');
               appendColumnName(buf, column);
               buf.append(" is ");
               m_adapter.appendLiteral(buf, sDescription);
               m_appender.appendSQL(buf.toString());
            }

            //Add LO management, Reference http://www.postgresql.org/docs/9.0/static/lo.html
            if (isBLOBColumn(column))
            {
               buf.setLength(0);
               buf.append("create trigger ");
               buf.append(column.getName()).append("_lo_manage");
               buf.append(" before update or delete on ");
               appendTableName(buf, table);
               buf.append(" for each row execute procedure lo_manage(");
               appendColumnName(buf, column);
               buf.append(')');
               m_appender.appendSQL(buf.toString());
            }
         }
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#dropColumn(nexj.core.meta.persistence.sql.Column)
    */
   protected void dropColumn(Column column)
   {
      Table table = column.getTable();

      Table[] views = null;

      if (table.getType() == Table.MANAGED)
      {
         views = getInverseDependency(table);

         for (int i = 0; i < views.length; ++i)
         {
            dropTable(views[i], false);
         }

         Index index = getFullTextSearchIndex(column);

         if (index != null)
         {
            dropFullTextIndex(index, true);
         }

         if (isBLOBColumn(column))
         {
            unlinkLargeObjects(column); // unlink large objects
         }
      }

      super.dropColumn(column);

      if (views != null)
      {
         for (int i = 0; i < views.length; ++i)
         {
            createTable(views[i]);
         }
      }
   }

   /**
    * Drops a full-text search index.
    * @param index The full-text search index to drop.
    * @param bDropFullTextColumn True to drop full-text column as well.
    */
   protected void dropFullTextIndex(Index index, boolean bDropFullTextColumn)
   {
      Column column = index.getIndexColumn(0).getColumn();

      StringBuffer buf = new StringBuffer(128);
      Table table = column.getTable();

      dropFullTextTriggerProcedure(index);

      if (bDropFullTextColumn)
      {
         buf.append("alter table ");
         appendTableName(buf, table);
         buf.append(" drop column ");
         buf.append(column.getName());
         buf.append('_');
      }

      m_appender.appendSQL(buf.toString());
   }

   /**
    * Drops the full-text trigger procedure for the given column.
    * @param column The column name whose full-text trigger to drop.
    */
   protected void dropFullTextTriggerProcedure(Index index)
   {
      StringBuffer buf = new StringBuffer(128);
      Column column = index.getIndexColumn(0).getColumn();
      Table table = column.getTable();

      buf.append("drop trigger ");
      buf.append(column.getName());
      buf.append("_ on ");
      appendTableName(buf, table);
      m_appender.appendSQL(buf.toString());

      buf.setLength(0);
      buf.append("drop function ");
      appendFullTextProcedureName(buf, table, index, true);
      buf.append("()");

      m_appender.appendSQL(buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#dropIndex(nexj.core.meta.persistence.sql.Index)
    */
   protected void dropIndex(Index index)
   {
      Table table = index.getTable();

      if (table.getType() == Table.MANAGED && isFullTextSearchIndex(index))
      {
         dropFullTextIndex(index, true);
      }

      if (!m_bClouded)
      {
         super.dropIndex(index);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#dropTable(nexj.core.meta.persistence.sql.Table, boolean)
    */
   public void dropTable(Table table, boolean bAll)
   {
      byte nTableType = table.getType();

      if (nTableType == Table.MANAGED ||
          nTableType == Table.QUERY ||
          nTableType == Table.VIEW)
      {
         Table[] views = getInverseDependency(table);

         if (views.length != 0)
         {
            StringBuffer buf = new StringBuffer(128);

            for (int i = 0; i < views.length; ++i)
            {
               dropTable(views[i], bAll);
               buf.setLength(0);
               buf.append("create view ");
               appendTableName(buf, views[i]);
               buf.append(" as select 1");
               m_appender.appendSQL(buf.toString());
            }
         }

         if (nTableType == Table.MANAGED)
         {
            unlinkLargeObjects(table);

            for (int i = table.getIndexCount() - 1; i > -1; --i)
            {
               Index index = table.getIndex(i);

               if (isFullTextSearchIndex(index))
               {
                  dropFullTextTriggerProcedure(index);
               }
            }
         }
      }

      super.dropTable(table, bAll);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#truncateTable(nexj.core.meta.persistence.sql.Table)
    */
   public void truncateTable(Table table)
   {
      if (m_bClouded && table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(64);

         buf.append("delete from ");
         appendTableName(buf, table);
         m_appender.appendSQL(buf.toString());

         buf.setLength(0);
         buf.append("vacuum ");
         appendTableName(buf, table);
         m_appender.appendSQL(buf.toString());
      }
      else
      {
         unlinkLargeObjects(table);
         super.truncateTable(table);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getAddColumnToken()
    */
   protected String getAddColumnToken()
   {
      return " add column ";
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#getAlterColumnSeparator()
    */
   protected String getAlterColumnSeparator()
   {
     return ",";
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#getCaseSensitiveName(java.lang.String)
    */
   protected String getCaseSensitiveName(String sName)
   {
      //PostgreSQL adds quotes to columns that have $ in them, so strip them off first
      return super.getCaseSensitiveName(toMetadataCase(sName));
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getCreateEtcScriptName()
    */
   protected String getCreateEtcScriptName()
   {
      return (m_bClouded) ? null : "postgresql_create.sql";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDropEtcScriptName()
    */
   protected String getDropEtcScriptName()
   {
      return (m_bClouded) ? null : "postgresql_drop.sql";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultTablespace()
    */
   protected String getDefaultTablespace()
   {
      return "nexjdata01";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultIndexspace()
    */
   protected String getDefaultIndexspace()
   {
      return "nexjindx01";
   }

   /**
    * Tablespace not supported for largeobject tables.
    *
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultLongspace()
    */
   protected String getDefaultLongspace()
   {
      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getDefaultOwner()
    */
   public String getDefaultOwner()
   {
      return "nexjsa";
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
            buf.append(getSeparator());
         }
      };
   }

   /**
    * Appends a full-text search procedure name to a string buffer.
    * @param buf The destination buffer.
    * @param table The table that the column belongs to.
    * @param index The full text search index.
    * @param bOwner True to include the table owner.
    * @return The full-text search procedure name.
    */
   protected String appendFullTextProcedureName(StringBuffer buf, Table table, Index index, boolean bOwner)
   {
      if (bOwner)
      {
         String sOwner = table.getOwnerName();

         if (sOwner == null)
         {
            sOwner = getOwner();
         }

         if (sOwner != null)
         {
            buf.append(sOwner);
            buf.append('.');
         }
      }

      // ProcedureName tsvectorupdate_tableName_columnName_
      buf.append("tsvectorupdate_");
      buf.append(table.getTableName());
      buf.append('_');
      buf.append(index.getIndexColumn(0).getColumn().getName());
      buf.append('_');

      return buf.toString();
   }

   /**
    * For a give column, returns a its full-text search index if any.
    * @param column The column.
    * @return the full-text search index else null.
    */
   protected Index getFullTextSearchIndex(Column column)
   {
      Table table = column.getTable();

      for (int i = table.getIndexCount() - 1; i >= 0; --i) // backwards due to index removal
      {
         Index index = table.getIndex(i);

         if (isFullTextSearchIndex(index) && index.findIndexColumn(column) != null)
         {
            return index;
         }
      }

      return null;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getGUIDExpr()
    */
   protected String getGUIDExpr()
   {
      return "uuid_generate_v4()";
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getIndexName(nexj.core.meta.persistence.sql.Index, boolean,boolean)
    */
   protected String getIndexName(Index index, boolean bConstraint, boolean bQuote)
   {
      String sTableName = index.getTable().getTableName();
      String sIndexName = index.getName();

      if (sTableName != null)
      {
         if (!sIndexName.startsWith(sTableName) && !(sIndexName.contains(".") || sIndexName.contains("_")))
         {
            sIndexName = sTableName + '.' + sIndexName; //index.getName(Table) does not work because it uses table.getName() not table.getTableName()
         }
      }

      return getIndexName(sIndexName, "", "", bConstraint, bQuote);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getIndexName(java.lang.String, java.lang.String, java.lang.String, boolean, boolean)
    */
   protected String getIndexName(String sName, String sPrefix, String sOwner, boolean bConstraint, boolean bQuote)
   {
      StringBuffer buf = new StringBuffer(32);

      if (sPrefix != null)
      {
         buf.append(sPrefix);
      }

      for (int i = 0, nCount = sName.length(); i < nCount; ++i)
      {
         char ch = sName.charAt(i);

         if (ch == '.')
         {
            ch = '_';
         }

         buf.append(ch);
      }

      if (bQuote)
      {
         return quote(buf.toString());
      }

      return buf.toString();
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
      return SysUtil.LINE_SEP + ';' + SysUtil.LINE_SEP;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getSetupEtcScriptName()
    */
   protected String getSetupEtcScriptName()
   {
      return "postgresql_setup.sql";
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#init(nexj.core.meta.persistence.sql.RelationalSchema)
    */
   protected void init(RelationalSchema schema)
   {
      super.init(schema);

      m_schema = schema;
   }

   /**
    * Checks whether a given column's type is BLOB.
    * @param column The column.
    * @return true if column type is BLOB.
    */
   protected boolean isBLOBColumn(Column column)
   {
      return !m_bClouded &&
         column.getType() == Primitive.BINARY &&
         column.isLOB(0, PostgreSQLAdapter.MAX_BYTEA_PRECISION);
   }

   /**
    * @see nexj.core.persistence.sql.CaseInsensitiveSQLSchemaManager#isCaseInsensitive(java.lang.String)
    */
   protected boolean isCaseInsensitive(String sName)
   {
      int i = sName.length();

      return i > 1 && sName.charAt(i - 2) == '$'; //check len-2 because column names that contain $ are wrapped with quotes
   }

   /**
    * Checks if a given index is a full-text search index.
    * @param index The index.
    * @return true if index is full-text search index.
    */
   protected boolean isFullTextSearchIndex(Index index)
   {
      return index.getType() == Index.TEXT && !m_bClouded;
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
         return true;
      }

      if (dstType == Primitive.BINARY || srcType == Primitive.BINARY)
      {
         return false;
      }

      if (dstType == Primitive.BOOLEAN || srcType == Primitive.BOOLEAN)
      {
         return false; // must implicitly cast
      }

     return true;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#isValidColumnName(java.lang.String)
    */
   protected boolean isValidColumnName(String sName)
   {
      return super.isValidColumnName(sName) && sName.charAt(sName.length() - 1) != '_';
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#isWindowsCompatible(nexj.core.util.PropertyMap)
    */
   protected boolean isWindowsCompatible(PropertyMap config)
   {
      return false; //postgresql uses unix style paths c:/path/to/folder
   }

   /**
    * Alters a column to be nullable.
    * @param column The column which is to be made nullable.
    */
   protected void makeColumnNullable(Column column)
   {
      Table table = column.getTable();
      StringBuffer buf = new StringBuffer(128);

      buf.append("alter table ");
      appendTableName(buf, table);
      buf.append(" alter column ");
      appendColumnName(buf, column);
      buf.append(" drop not null");

      m_appender.appendSQL(buf.toString());
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#setType(nexj.core.meta.persistence.sql.Column, java.lang.String, int, int, int)
    */
   protected void setType(Column column, String sType, int nType, int nPrecision, int nScale)
   {
      byte nAllocation = Column.FIXED;
      Primitive type = null;

      switch (nType)
      {
         case Types.BINARY:
            type = Primitive.BINARY;
            nPrecision = 1; // PostgreSQL incorrectly reports precision as Integer.MAX_VALUE

            break;

         case Types.VARCHAR:
            type = Primitive.STRING;
            nAllocation = sType.equals("text") ? Column.LOCATOR : Column.VARYING;

            break;

         case Types.DISTINCT:
            if (sType.equals("lo"))
            {
               type = Primitive.BINARY;
               nAllocation = Column.LOCATOR;
            }

            break;

         default:
            super.setType(column, sType, nType, nPrecision, nScale);

            return;
      }

      if (type != null)
      {
         column.setType(type);
         column.setPrecision(nPrecision);
         column.setScale(0);
         column.setAllocation(nAllocation);
      }
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
      buf.append(" rename column ");
      appendColumnName(buf, oldColumn);
      buf.append(" to ");
      appendColumnName(buf, newColumn);
      m_appender.appendSQL(buf.toString());

      // if column has full-text, drop trigger, rename tsvector column and create trigger
      Index oldIndex = getFullTextSearchIndex(oldColumn);

      if (oldIndex != null)
      {
         Index newIndex = getFullTextSearchIndex(newColumn);

         dropFullTextIndex(oldIndex, false);

         buf.setLength(0);
         buf.append("alter table ");
         appendTableName(buf, table);
         buf.append(" rename column ");
         buf.append(oldColumn.getName());
         buf.append("_ to ");
         appendColumnName(buf, newColumn);
         m_appender.appendSQL(buf.toString());

         createFullTextSearchTrigger(newIndex);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#renameIndex(nexj.core.meta.persistence.sql.Index, nexj.core.meta.persistence.sql.Index)
    */
   protected void renameIndex(Index newIndex, Index oldIndex)
   {
      Table table = oldIndex.getTable();

      if (table.getType() != Table.MANAGED || oldIndex.getType() < Index.TEXT)
      {
         super.renameIndex(newIndex, oldIndex);

         return;
      }

      if (!m_bClouded || isFullTextSearchIndex(oldIndex))
      {
         boolean bConstraint = isPrimaryKey(oldIndex);
         StringBuffer buf = new StringBuffer(128);

         buf.append("alter index ");
         buf.append(getIndexName(oldIndex, bConstraint , true));
         buf.append(" rename to ");
         buf.append(getIndexName(newIndex, bConstraint, true));

         m_appender.appendSQL(buf.toString());
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#renameTable(Table, Table)
    */
   protected void renameTable(Table newTable, Table oldTable)
   {
      if (oldTable.getType() != Table.MANAGED)
      {
         super.renameTable(newTable, oldTable);

         return;
      }

      // ALTER TABLE OldTable RENAME TO NewTable
      // views are updated to point to NewTable
      StringBuffer buf = new StringBuffer(64);

      buf.append("alter table ");
      appendTableName(buf, oldTable);
      buf.append(" rename to ");
      buf.append(quote(newTable.getTableName()));
      m_appender.appendSQL(buf.toString());

      // rename full-text trigger Procedure name
      for (int i = oldTable.getIndexCount() - 1; i > -1; --i)
      {
         Index index = oldTable.getIndex(i);

         if (isFullTextSearchIndex(index))
         {
            buf.setLength(0);
            buf.append("alter function ");
            appendFullTextProcedureName(buf, oldTable, index, true);
            buf.append("rename to ");
            appendFullTextProcedureName(buf, newTable, index, false);

            m_appender.appendSQL(buf.toString());
         }
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#setCustomDatabaseProperties(nexj.core.util.PropertyMap, nexj.core.util.PropertyMap)
    */
   public PropertyMap setCustomDatabaseProperties(PropertyMap map, PropertyMap customMap)
   {
      map.setValue("contribpath", (customMap == null) ? null : customMap.findValue("contribpath"));
      return super.setCustomDatabaseProperties(map, customMap);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#toDatabaseCase(java.lang.String)
    */
   protected String toDatabaseCase(String sName)
   {
      if (sName == null)
      {
         return sName;
      }

      return sName.toLowerCase(Locale.ENGLISH);
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#toMetadataCase(java.lang.String)
    */
   protected String toMetadataCase(String sName)
   {
      // PostgreSQL quotes some names, so strip the quotes
      if ( sName.charAt(0) == '"' && sName.charAt(sName.length() - 1) == '"')
      {
         sName = sName.substring(1, sName.length() - 1);
      }

      return super.toMetadataCase(sName);
   }

   /**
    * Unlinks all large objects from the given column.
    * Reference http://www.postgresql.org/docs/9.0/static/lo.html
    * @param column The column to delete large objects from.
    */
   protected void unlinkLargeObjects(Column column)
   {
      StringBuffer buf = new StringBuffer(128);

      if (!column.isNullable())
      {
         makeColumnNullable(column);
      }

      appendUpdateColumnRowsToNull(buf,column);

      m_appender.appendSQL(buf.toString());
   }

   /**
    * Unlinks all large objects from the given table.
    * Reference http://www.postgresql.org/docs/9.0/static/lo.html
    * @param columns The table for which large objects should be deleted.
    */
   protected void unlinkLargeObjects(Table table)
   {
      StringBuffer buf = null;
      boolean bHasBLOB = false;
      Column column;

      for (int i = table.getColumnCount() - 1; i >= 0; --i)
      {
         column = table.getColumn(i);

         if (isBLOBColumn(column))
         {
            if (!bHasBLOB)
            {
               bHasBLOB = true;
               buf = new StringBuffer(128);

               appendUpdateColumnRowsToNull(buf,column);
            }
            else
            {
               buf.append(", ");
               appendColumnName(buf, column);
               buf.append(" = null");
            }

            if (!column.isNullable()) // make the column null so that large objects can be set to null
            {
               makeColumnNullable(column);
            }
         }
      }

      if (bHasBLOB)
      {
         m_appender.appendSQL(buf.toString());
      }
   }
}