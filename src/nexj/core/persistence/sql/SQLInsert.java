// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.OID;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Work;
import nexj.core.persistence.sql.SQLAdapter.Bind;
import nexj.core.runtime.Instance;
import nexj.core.util.Undefined;

/**
 * SQL insert work item.
 */
public final class SQLInsert extends SQLWork
{
   // attributes

   /**
    * The identity generator flag.
    */
   private boolean m_bIdentity;
   
   /**
    * The callable flag.
    */
   private boolean m_bCallable;
   
   /**
    * The bind variable count.
    */
   protected int m_nBindCount;

   // constructors
   
   /**
    * Constructs the SQLInsert work item.
    * @see SQLWork
    */
   protected SQLInsert(Instance instance, Table table, SQLAdapter adapter)
   {
      super(instance, table, adapter);
   }

   // operations
   
   /**
    * @see nexj.core.persistence.Work#getType()
    */
   public int getType()
   {
      return INSERT;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#setIdentity(boolean)
    */
   public void setIdentity(boolean bIdentity)
   {
      m_bIdentity = bIdentity;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#isIdentity()
    */
   public boolean isIdentity()
   {
      return m_bIdentity;
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#isCallable()
    */
   public boolean isCallable()
   {
      return m_bCallable;
   }

   /**
    * Sets the bind variable count.
    * @param nBindCount The bind variable count to set.
    */
   public void setBindCount(int nBindCount)
   {
      m_nBindCount = nBindCount;
   }

   /**
    * @return The bind variable count.
    */
   public int getBindCount()
   {
      return m_nBindCount;
   }
   
   /**
    * @see nexj.core.persistence.sql.SQLWork#getSQL()
    */
   public String getSQL()
   {
      StringBuffer buf = new StringBuffer(128);
      int nValCount = 0;
      
      if (m_bIdentity)
      {
         m_adapter.appendIdentityPrefix(buf, this);
      }

      buf.append("insert into ");
      m_adapter.appendTable(buf, m_table);
      buf.append('(');
   
      if (m_bIdentity && m_adapter.appendIdentityColumn(buf, this))
      {
         ++nValCount;
      }

      int nCount = m_table.getColumnCount();
      boolean bCaseConverted = false;

      for (int i = 0; i < nCount; ++i)
      {
         if (getValue(i) != Undefined.VALUE)
         {
            if (nValCount++ > 0)
            {
               buf.append(", ");
            }
            
            Column column = m_table.getColumn(i);

            m_adapter.appendColumn(buf, column);

            if (m_adapter.isCaseConverted(column))
            {
               buf.append(", ");
               m_adapter.appendCaseConvertedColumn(buf, column);
               bCaseConverted = true;
            }
         }
      }

      if (nValCount == 0)
      {
         buf.setLength(buf.length() - 1);
         buf.append(" default values");
      }
      else
      {
         buf.append(") values (");
                        
         if (m_bIdentity && m_adapter.appendIdentityValue(buf, this))
         {
            if (--nValCount > 0)
            {
               buf.append(", ");
            }
         }

         if (bCaseConverted)
         {
            nValCount = 0;

            for (int i = 0; i < nCount; ++i)
            {
               if (getValue(i) != Undefined.VALUE)
               {
                  if (nValCount != 0)
                  {
                     buf.append(", ");
                  }

                  m_adapter.appendBind(buf, nValCount++);

                  Column column = m_table.getColumn(i);

                  if (m_adapter.isCaseConverted(column))
                  {
                     buf.append(", ");
                     m_adapter.appendCaseConvertedBind(buf, nValCount++, column);
                  }
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

               m_adapter.appendBind(buf, i);
            }
         }

         buf.append(')');
      }

      m_nBindCount = nValCount;

      if (m_bIdentity)
      {
         m_bCallable = m_adapter.appendIdentitySuffix(buf, this);
      }

      return buf.toString();
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#bind(java.sql.PreparedStatement, SQLWork)
    */
   public void bind(PreparedStatement stmt, SQLWork proto) throws SQLException
   {
      int nValCount = 0;
      int nCount = m_table.getColumnCount();

      for (int i = 0; i < nCount; ++i)
      {
         Object value = getValue(i);

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

      if (m_bIdentity)
      {
         m_adapter.bindIdentity(stmt, this);
      }
   }


   /**
    * @see nexj.core.persistence.sql.SQLWork#isBatchable()
    */
   public boolean isBatchable()
   {
      return !m_bIdentity && m_adapter.isBatchable(this);
   }

   /**
    * @see nexj.core.persistence.sql.SQLWork#execute(java.sql.PreparedStatement, nexj.core.persistence.sql.Work[], int, int)
    */
   public void execute(PreparedStatement stmt, Work[] workArray, int nStart, int nEnd) throws SQLException
   {
      if (m_bIdentity)
      {
         stmt.execute();

         Column column = m_table.getPrimaryKey().getIndexColumn(0).getColumn();

         setInstanceValue(column, m_adapter.toValue(column, m_adapter.getIdentityValue(stmt, column, this)));
         m_instance.setOID(computeOID());
         fixup();
      }
      else
      {
         for (int i = nStart; i < nEnd; ++i)
         {
            SQLWork work = (SQLWork)workArray[i];
                              
            Instance instance = work.getInstance();

            if (instance.getOID() == null)
            {
               OID oid = work.computeOID();

               if (oid == null)
               {
                  throw new PersistenceException("err.persistence.requiredOID",
                     new Object[]{instance.getLazyMetaclass()});
               }

               instance.setOID(oid);
            }

            work.fixup();
         }

         if (nEnd - nStart > 1)
         {
            m_adapter.executeBatch(stmt);
         }
         else
         {
            m_adapter.executeUpdate(stmt);
         }
      }
   }
}
