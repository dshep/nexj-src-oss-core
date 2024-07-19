// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.OID;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.Work;
import nexj.core.persistence.sql.SQLAdapter.Bind;
import nexj.core.runtime.Instance;
import nexj.core.util.RandUtil;
import nexj.core.util.Undefined;

/**
 * SQL update work item.
 */
public final class SQLUpdate extends SQLWork
{
   // attributes

   /**
    * True if the primary key has changed.
    */
   private boolean m_bKeyChanged;

   /**
    * True if a conditional for handling no rows is generated.
    */
   private boolean m_bNoRowsBlock;

   /**
    * True if a standalone insert statement has to be executed.
    */
   private boolean m_bInsert;

   /**
    * True if a denormalized insert has been generated.
    */
   private boolean m_bDenorm;

   /**
    * The original lock value.
    */
   private Object m_lockValue = Undefined.VALUE;
   
   // associations

   /**
    * The column corresponding to the locking attribute.
    */
   private Column m_lockingColumn;

   // constructors
   
   /**
    * Constructs the work item.
    * @see SQLWork
    */
   protected SQLUpdate(Instance instance, Table table, SQLAdapter adapter)
   {
      super(instance, table, adapter);
      m_bEmpty = true;
   }

   // operations

   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return UPDATE;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#setLockingColumn(nexj.core.meta.persistence.sql.Column)
    */
   protected void setLockingColumn(Column column)
   {
      m_lockingColumn = column;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#getLockingColumn()
    */
   protected Column getLockingColumn()
   {
      return m_lockingColumn;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#setValue(nexj.core.meta.persistence.sql.Column, java.lang.Object)
    */
   public void setValue(Column column, Object value)
   {
      super.setValue(column, value);

      if (column != m_lockingColumn)
      {
         m_bEmpty = false;
      }
   }

   /**
    * Clears the empty flag if there is a locking attribute.
    */
   public void touch()
   {
      if (m_bEmpty && m_lockingColumn != null && getValue(m_lockingColumn) != Undefined.VALUE)
      {
         m_bEmpty = false;
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#isCompound()
    */
   public boolean isCompound()
   {
      if (!isPrimary())
      {
         int nCount = m_table.getColumnCount();
         
         for (int i = 0; i < nCount; ++i)
         {
            Column col = m_table.getColumn(i);

            if (!col.isNullable() && !col.isPrimary() && !col.isDenormalized())
            {
               Object value = getValue(i);

               if (value == Undefined.VALUE || value == null)
               {
                  return false;
               }
            }
         }
         
         return true;
      }

      return false;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#getSQL()
    */
   public String getSQL()
   {
      StringBuffer buf = new StringBuffer(128);
      boolean bPrimary = isPrimary();

      if (!bPrimary)
      {
         m_bNoRowsBlock = isCompound();

         if (m_bNoRowsBlock)
         {
            m_bNoRowsBlock = m_adapter.appendNoRowsBlock(buf);
            m_bInsert = !m_bNoRowsBlock;
         }
      }

      buf.append("update ");
      m_adapter.appendTable(buf, m_table);
      buf.append(" set ");

      int nCount = m_table.getColumnCount();
      int nValCount = 0;

      for (int i = 0; i < nCount; ++i)
      {
         if (getValue(i) != Undefined.VALUE)
         {
            if (nValCount > 0)
            {
               buf.append(", ");
            }

            Column column = m_table.getColumn(i);

            m_adapter.appendColumn(buf, column);
            buf.append(" = ");
            m_adapter.appendBind(buf, nValCount++);

            if (m_adapter.isCaseConverted(column))
            {
               buf.append(", ");
               m_adapter.appendCaseConvertedColumn(buf, column);
               buf.append(" = ");
               m_adapter.appendCaseConvertedBind(buf, nValCount++, column);
            }
         }
         else if (!bPrimary && !m_bDenorm && findSource(m_table.getColumn(i)) != null)
         {
            m_bDenorm = true;
         }
      }

      buf.append(" where ");

      Index pk = m_table.getPrimaryKey();

      nCount = pk.getIndexColumnCount();

      for (int i = 0; i < nCount; ++i)
      {
         if (i > 0)
         {
            buf.append(" and ");
         }

         m_adapter.appendColumn(buf, pk.getIndexColumn(i).getColumn());
         buf.append(" = ");
         m_adapter.appendBind(buf, nValCount++);
      }

      if (m_lockingColumn != null)
      {
         buf.append(" and ");
         m_adapter.appendColumn(buf, m_lockingColumn);
         buf.append(" = ");
         m_adapter.appendBind(buf, nValCount++);
      }

      if (bPrimary)
      {
         if (((RelationalMapping)m_mapping).getKeyGenerator() == null)
         {
            for (int i = 0; i < nCount; ++i)
            {
               if (getValue(pk.getIndexColumn(i).getColumn()) != Undefined.VALUE)
               {
                  m_bKeyChanged = true;
                  break;
               }
            }
         }
      }
      else
      {
         if (m_bNoRowsBlock)
         {
            m_adapter.appendNoRowsStart(buf);
            appendInsert(buf, nValCount);
            m_adapter.appendNoRowsEnd(buf);
         }
      }

      return buf.toString();
   }

   /**
    * Appends an insert SQL statement to a string buffer.
    * @param buf The statement output buffer.
    * @param nBindCount The count of the bind values output so far. 
    */
   private void appendInsert(StringBuffer buf, int nBindCount)
   {
      buf.append("insert into ");
      m_adapter.appendTable(buf, m_table);
      buf.append('(');

      Index pk = m_table.getPrimaryKey();
      int nValCount = 0;

      for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
      {
         Column column = pk.getIndexColumn(i).getColumn();

         if (getValue(column) == Undefined.VALUE)
         {
            if (nValCount++ > 0)
            {
               buf.append(", ");
            }

            m_adapter.appendColumn(buf, column);
         }
      }

      boolean bCaseConverted = false;

      for (int i = 0, n = m_table.getColumnCount(); i < n; ++i)
      {
         Column column = m_table.getColumn(i);

         if (getValue(i) != Undefined.VALUE ||
            m_bDenorm && findSource(column) != null)
         {
            if (nValCount++ > 0)
            {
               buf.append(", ");
            }

            m_adapter.appendColumn(buf, column);

            if (m_adapter.isCaseConverted(column))
            {
               buf.append(", ");
               m_adapter.appendCaseConvertedColumn(buf, column);
               bCaseConverted = true;
            }
         }
      }

      buf.append((m_bDenorm) ? ") select " : ") values (");

      if (bCaseConverted | m_bDenorm)
      {
         nValCount = 0;

         for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
         {
            Column column = pk.getIndexColumn(i).getColumn();

            if (getValue(column) == Undefined.VALUE)
            {
               if (nValCount > 0)
               {
                  buf.append(", ");
               }

               m_adapter.appendBind(buf, nBindCount + nValCount++, column);
            }
         }

         // TODO: Support more than just the primary table denorm
         Table denormTable = null;

         for (int i = 0, n = m_table.getColumnCount(); i < n; ++i)
         {
            Column column = m_table.getColumn(i);

            if (getValue(i) != Undefined.VALUE)
            {
               if (nValCount != 0 || denormTable != null)
               {
                  buf.append(", ");
               }

               m_adapter.appendBind(buf, nBindCount + nValCount++, column);

               if (m_adapter.isCaseConverted(column))
               {
                  buf.append(", ");
                  m_adapter.appendCaseConvertedBind(buf, nBindCount + nValCount++, column);
               }
            }
            else
            {
               Column srcColumn = findSource(column);

               if (srcColumn != null)
               {
                  if (nValCount != 0 || denormTable != null)
                  {
                     buf.append(", ");
                  }

                  buf.append("A.");
                  m_adapter.appendColumn(buf, srcColumn);

                  if (m_adapter.isCaseConverted(column))
                  {
                     buf.append(", ");
                     buf.append("A.");
                     m_adapter.appendCaseConvertedColumn(buf, srcColumn);
                  }

                  denormTable = srcColumn.getTable();
               }
            }
         }

         if (m_bDenorm)
         {
            buf.append(" from ");
            m_adapter.appendTable(buf, denormTable);
            buf.append(" A where ");

            for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
            {
               if (i != 0)
               {
                  buf.append(" and ");
               }

               Column column = pk.getIndexColumn(i).getColumn();

               buf.append("A.");
               m_adapter.appendColumn(buf, column);
               buf.append(" = ");

               m_adapter.appendBind(buf, nBindCount + nValCount++);
            }
         }
      }
      else
      {
         for (int i = 0; i < nValCount; ++i)
         {
            if (i > 0)
            {
               buf.append(", ");
            }
            
            m_adapter.appendBind(buf, nBindCount + i);
         }
      }

      if (!m_bDenorm)
      {
         buf.append(')');
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#bind(java.sql.PreparedStatement, SQLWork)
    */
   public void bind(PreparedStatement stmt, SQLWork proto) throws SQLException
   {
      SQLUpdate init = (SQLUpdate)proto;

      m_bKeyChanged = init.m_bKeyChanged;
      m_bNoRowsBlock = init.m_bNoRowsBlock;
      m_bInsert = init.m_bInsert;
      m_bDenorm = init.m_bDenorm;

      int nValCount = 0;
      Object value;

      for (int i = 0, n = m_table.getColumnCount(); i < n; ++i)
      {
         value = getValue(i);
         
         if (value != Undefined.VALUE)
         {
            Column column = m_table.getColumn(i);

            if (column == m_lockingColumn && m_lockValue == Undefined.VALUE)
            {
               m_lockValue = m_adapter.toBind(column, value);

               do
               {
                  value = column.getValueType().convert(Primitive.createInteger(RandUtil.getSecureRandom().nextInt()));
               }
               while (value.equals(getValue(i)));

               setInstanceValue(column, value);
            }

            value = m_adapter.toBind(column, value);

            Bind bind = m_adapter.getBind(column);

            m_adapter.logBindValue(nValCount, value);
            bind.setValue(stmt, nValCount++, value, m_adapter);

            if (m_adapter.isCaseConverted(column))
            {
               m_adapter.logBindValue(nValCount, value);
               bind.setValue(stmt, nValCount++, value, m_adapter);
            }
         }
      }

      Index pk = m_table.getPrimaryKey();
      OID oid = m_instance.getOID(); 

      for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
      {
         Column column = pk.getIndexColumn(i).getColumn();

         value = m_adapter.toBind(column, oid.getValue(i));
         m_adapter.logBindValue(nValCount, value);
         m_adapter.getBind(column).setValue(stmt, nValCount++, value, m_adapter);
      }

      if (m_lockingColumn != null)
      {
         m_adapter.logBindValue(nValCount, m_lockValue);
         m_adapter.getBind(m_lockingColumn).setValue(stmt, nValCount, m_lockValue, m_adapter);
      }

      if (m_bNoRowsBlock)
      {
         bindInsert(stmt, nValCount);
      }
   }

   /**
    * Sets the bind values for the prepared insert statement.
    * @param stmt The prepared statement.
    * @param nValCount The count of the already set bind values. 
    */
   private void bindInsert(PreparedStatement stmt, int nValCount) throws SQLException
   {
      Index pk = m_table.getPrimaryKey();
      OID oid = m_instance.getOID();
      Object value; 

      for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
      {
         Column column = pk.getIndexColumn(i).getColumn();

         if (getValue(column) == Undefined.VALUE)
         {
            value = m_adapter.toBind(column, oid.getValue(i));
            m_adapter.logBindValue(nValCount, value);
            m_adapter.getBind(column).setValue(stmt, nValCount++, value, m_adapter);
         }
      }

      for (int i = 0, n = m_table.getColumnCount(); i < n; ++i)
      {
         value = getValue(i);
         
         if (value != Undefined.VALUE)
         {
            Column column = m_table.getColumn(i);

            value = m_adapter.toBind(column, value);

            Bind bind = m_adapter.getBind(column);

            m_adapter.logBindValue(nValCount, value);
            bind.setValue(stmt, nValCount++, value, m_adapter);

            if (m_adapter.isCaseConverted(column))
            {
               m_adapter.logBindValue(nValCount, value);
               bind.setValue(stmt, nValCount++, value, m_adapter);
            }
         }
      }

      if (m_bDenorm)
      {
         for (int i = 0, n = pk.getIndexColumnCount(); i < n; ++i)
         {
            Column column = pk.getIndexColumn(i).getColumn();
   
            value = m_adapter.toBind(column, oid.getValue(i));
            m_adapter.logBindValue(nValCount, value);
            m_adapter.getBind(column).setValue(stmt, nValCount++, value, m_adapter);
         }
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#isBatchable()
    */
   public boolean isBatchable()
   {
      return (m_lockingColumn == null && !m_bInsert ||
         m_adapter.isBatchUpdateCountSupported()) &&
         m_adapter.isBatchable(this);
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#execute(java.sql.PreparedStatement, nexj.core.persistence.sql.Work[], int, int)
    */
   public void execute(PreparedStatement stmt, Work[] workArray, int nStart, int nEnd) throws SQLException
   {
      boolean bInsert = false;
      
      try
      {
         if (nEnd - nStart > 1)
         {
            int[] updateCounts = m_adapter.executeBatch(stmt);
            int nCount = updateCounts.length;

            for (int i = 0; i < nCount; ++i)
            {
               if (updateCounts[i] != 1 && (m_lockingColumn != null || m_bInsert || updateCounts[i] != PreparedStatement.SUCCESS_NO_INFO))
               {
                  if (isPrimary())
                  {
                     throw new OptimisticLockException(workArray[nStart + i].getInstance());
                  }
                  else if (m_bInsert)
                  {
                     if (!bInsert)
                     {
                        stmt = stmt.getConnection().prepareStatement(getInsertSQL());
                        bInsert = true;
                     }

                     ((SQLUpdate)workArray[nStart + i]).bindInsert(stmt, 0);
                     stmt.addBatch();                     
                  }
               }

               if (m_bKeyChanged)
               {
                  ((SQLUpdate)workArray[nStart + i]).updateOID();
               }
            }

            if (bInsert)
            {
               m_adapter.executeBatch(stmt);
            }
         }
         else
         {
            if (m_adapter.executeUpdate(stmt) != 1)
            {
               if (isPrimary())
               {
                  throw new OptimisticLockException(m_instance);
               }
               else if (m_bInsert)
               {
                  stmt = stmt.getConnection().prepareStatement(getInsertSQL());
                  bInsert = true;
                  bindInsert(stmt, 0);
                  m_adapter.executeUpdate(stmt);
               }
            }

            if (m_bKeyChanged)
            {
               updateOID();
            }
         }
      }
      finally
      {
         if (bInsert)
         {
            m_adapter.close(stmt);
         }
      }
   }

   /**
    * Generates a stand-alone insert SQL statement.
    * @return The generated SQL statement.
    */
   private String getInsertSQL()
   {
      StringBuffer buf = new StringBuffer(128);

      appendInsert(buf, 0);

      String sSQL = buf.toString();

      m_adapter.log(sSQL);

      return sSQL;
   }
   
   /**
    * Updates the OID in the work item instance.
    */
   private void updateOID()
   {
      m_instance.setOID(computeOID());
   }
}
