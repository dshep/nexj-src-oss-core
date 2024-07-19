// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.OptimisticLockException;
import nexj.core.persistence.Work;
import nexj.core.runtime.Instance;

/**
 * SQL delete work item.
 */
public final class SQLDelete extends SQLWork
{
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
   protected SQLDelete(Instance instance, Table table, SQLAdapter adapter)
   {
      super(instance, table, adapter);
   }

   // operations

   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return DELETE;
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
    * @see nexj.core.persistence.sql.SQLWork#getSQL()
    */
   public String getSQL()
   {
      StringBuffer buf = new StringBuffer(64);
      
      buf.append("delete from ");
      m_adapter.appendTable(buf, m_table);
      buf.append(" where ");
      
      Index pk = m_table.getPrimaryKey();
      int nCount = pk.getIndexColumnCount();
      
      for (int i = 0; i < nCount; ++i)
      {
         if (i > 0)
         {
            buf.append(" and ");
         }

         Column column = pk.getIndexColumn(i).getColumn();

         m_adapter.appendColumn(buf, column);
         buf.append(" = ");
         m_adapter.appendBind(buf, i);
      }
      
      if (m_lockingColumn != null)
      {
         buf.append(" and ");
         m_adapter.appendColumn(buf, m_lockingColumn);
         buf.append(" = ");
         m_adapter.appendBind(buf, nCount);
      }

      return buf.toString();
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#bind(java.sql.PreparedStatement, SQLWork)
    */
   public void bind(PreparedStatement stmt, SQLWork proto) throws SQLException
   {
      Index pk = m_table.getPrimaryKey();
      int nCount = pk.getIndexColumnCount();
      Object value;

      for (int i = 0; i < nCount; ++i)
      {
         Column column = pk.getIndexColumn(i).getColumn();

         value = m_adapter.toBind(column, getValue(column));
         m_adapter.logBindValue(i, value);
         m_adapter.getBind(column).setValue(stmt, i, value, m_adapter);
      }
      
      if (m_lockingColumn != null)
      {
         value = m_adapter.toBind(m_lockingColumn, getValue(m_lockingColumn));
         m_adapter.logBindValue(nCount, value);
         m_adapter.getBind(m_lockingColumn).setValue(stmt, nCount, value, m_adapter);
      }
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#isBatchable()
    */
   public boolean isBatchable()
   {
      return m_lockingColumn == null || m_adapter.isBatchUpdateCountSupported();
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#execute(java.sql.PreparedStatement, nexj.core.persistence.sql.Work[], int, int)
    */
   public void execute(PreparedStatement stmt, Work[] workArray, int nStart, int nEnd) throws SQLException
   {
      if (nEnd - nStart > 1)
      {
         int[] updateCounts = m_adapter.executeBatch(stmt);

         if (isPrimary())
         {
            int nCount = updateCounts.length;
            
            for (int i = 0; i < nCount; ++i)
            {
               if (updateCounts[i] != 1 && (m_lockingColumn != null || updateCounts[i] != PreparedStatement.SUCCESS_NO_INFO))
               {
                  throw new OptimisticLockException(workArray[nStart + i].getInstance());
               }
            }
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
         }
      }
   }
}
