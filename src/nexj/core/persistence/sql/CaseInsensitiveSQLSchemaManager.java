// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.Primitive;
import nexj.core.meta.PrimitivePropertyInitializer;
import nexj.core.meta.PropertyInitializer;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.RelationalSchema;
import nexj.core.meta.persistence.sql.Table;

/**
 * Base class for schema managers implementing case-insensitivity.
 */
public abstract class CaseInsensitiveSQLSchemaManager extends SQLSchemaManager
{
   // attributes

   /**
    * True if the data source is case-insensitive
    */
   protected boolean m_bCaseInsensitive = true;

   // constructors

   /**
    * Constructs the schema manager.
    * @param adapter The persistence adapter.
    */
   protected CaseInsensitiveSQLSchemaManager(SQLAdapter adapter)
   {
      super(adapter);
   }

   // operations

   /**
    * @see SQLSchemaManager#init(RelationalSchema)
    */
   protected void init(RelationalSchema schema)
   {
      super.init(schema);

      PropertyInitializer init = schema.getDataSource().getComponent().findPropertyInitializer("caseInsensitive");

      if (!(init instanceof PrimitivePropertyInitializer) ||
         ((PrimitivePropertyInitializer)init).getValue() == null ||
         ((PrimitivePropertyInitializer)init).getValue().equals(""))
      {
         m_bCaseInsensitive = true;
      }
      else
      {
         m_bCaseInsensitive = Primitive.toBoolean(((PrimitivePropertyInitializer)init).getValue()).booleanValue();
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#isCaseInsensitive(java.lang.String)
    */
   protected boolean isCaseInsensitive(String sName)
   {
      int i = sName.length();

      return i > 1 && sName.charAt(i - 1) == '$';
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#getCaseSensitiveName(java.lang.String)
    */
   protected String getCaseSensitiveName(String sName)
   {
      int i = sName.length();

      return (i > 0 && sName.charAt(i - 1) == '$') ? sName.substring(0, i -1) : sName;
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#isCaseInsensitive(nexj.core.meta.persistence.sql.Column)
    */
   protected boolean isCaseInsensitive(Column column)
   {
      return m_bCaseInsensitive && column.isCaseInsensitive();
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#alterColumn(nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column)
    */
   protected void alterColumn(Column newColumn, Column oldColumn)
   {
      Table table = newColumn.getTable();

      if (table.getType() == Table.MANAGED)
      {
         StringBuffer buf = new StringBuffer(128);

         // if CI column added or removed
         if (m_bCaseInsensitive && newColumn.isCaseInsensitive() != oldColumn.isCaseInsensitive())
         {
            buf.append("alter table ");
            appendTableName(buf, table);

            if (newColumn.isCaseInsensitive())
            {
               buf.append(getAddColumnToken());
               appendColumnDeclaration(buf, newColumn, "$", false, false);
               m_appender.appendSQL(buf.toString());

               buf.setLength(0);
               buf.append("update ");
               appendTableName(buf, table);
               buf.append(" set ");
               buf.append(newColumn.getName());
               buf.append('$');
               buf.append(" = ");
               appendColumnCaseConversion(buf, newColumn);
            }
            else
            {
               buf.append(getDropColumnToken());
               buf.append(oldColumn.getName());
               buf.append('$');
            }

            m_appender.appendSQL(buf.toString());
            oldColumn.setCaseInsensitive(newColumn.isCaseInsensitive());//update to current DB state
         }

         if (!isCompatible(newColumn, oldColumn)) // do not output alter if no difference in columns
         {
            if (m_bCaseInsensitive && newColumn.isCaseInsensitive()) // if have CI column
            {
               buf.setLength(0);
               buf.append("alter table ");
               appendTableName(buf, table);
               buf.append(getAlterColumnToken());
               appendColumnAlteration(buf, newColumn, oldColumn, null);
               buf.append(getAlterColumnSeparator());
               buf.append(getAlterColumnToken());
               appendColumnAlteration(buf, newColumn, oldColumn, "$");
               m_appender.appendSQL(buf.toString());
            }
            else
            {
               super.alterColumn(newColumn, oldColumn);
            }
         }
      }
   }

   /**
    * Also check case sensitivity.
    * @see nexj.core.persistence.sql.SQLSchemaManager#isCompatible(nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column)
    */
   protected boolean isCompatible(Column left, Column right)
   {
      return super.isCompatible(left, right) &&
             (!m_bCaseInsensitive || left.isCaseInsensitive() == right.isCaseInsensitive());
   }

   /**
    * Appends a case-converted column name to a string buffer.
    * @param buf The destination builder.
    * @param column The column to append.
    */
   protected abstract void appendColumnCaseConversion(StringBuffer buf, Column column);

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnAssignment(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendColumnAssignment(StringBuffer buf, Column dstColumn, Column srcColumn)
   {
      super.appendColumnAssignment(buf, dstColumn, srcColumn);

      if (isCaseInsensitive(dstColumn))
      {
         buf.append(", ");
         buf.append(dstColumn.getName());
         buf.append("$ = ");
         buf.append(srcColumn.getName());
         buf.append('$');
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, String)
    */
   protected void appendColumn(StringBuffer buf, Column column, String sSep)
   {
      super.appendColumn(buf, column, sSep);

      if (isCaseInsensitive(column))
      {
         buf.append(sSep);
         buf.append(column.getName());
         buf.append('$');
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, nexj.core.meta.persistence.sql.Column, java.lang.String)
    */
   protected void appendColumn(StringBuffer buf, Column column, Column target, String sSep)
   {
      super.appendColumn(buf, column, target, sSep);

      if (isCaseInsensitive(target))
      {
         buf.append(sSep);

         if (isCaseInsensitive(column))
         {
            buf.append(column.getName());
            buf.append('$');
         }
         else // need case converted version of column value
         {
            String sSuffix = ((CaseInsensitiveSQLAdapter)m_adapter)
                                .appendCaseConversion(buf, CaseInsensitiveSQLAdapter.CI_EXPR);

            super.appendColumn(buf, column, target, sSep);
            buf.append(sSuffix);
         }
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendColumnDeclaration(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column, boolean, boolean, String)
    */
   protected void appendColumnDeclaration(StringBuffer buf, Column column,
      boolean bNullability, boolean bCreateTable, String sSep)
   {
      super.appendColumnDeclaration(buf, column, bNullability, bCreateTable, sSep);

      if (isCaseInsensitive(column))
      {
         buf.append(sSep);
         appendColumnDeclaration(buf, column, "$", bNullability, bCreateTable);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLSchemaManager#appendIndexColumn(java.lang.StringBuffer, nexj.core.meta.persistence.sql.Column)
    */
   protected void appendIndexColumn(StringBuffer buf, Column column)
   {
      appendColumnName(buf, column, (isCaseInsensitive(column)) ? "$" : null);
   }
}
