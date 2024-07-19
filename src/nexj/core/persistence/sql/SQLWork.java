// Copyright 2010-2011 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Arrays;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.Key;
import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.RelationalDenorm;
import nexj.core.meta.persistence.sql.RelationalMapping;
import nexj.core.meta.persistence.sql.RelationalPrimitiveMapping;
import nexj.core.meta.persistence.sql.Table;
import nexj.core.persistence.OID;
import nexj.core.persistence.PersistenceAdapter;
import nexj.core.persistence.PersistenceException;
import nexj.core.persistence.Work;
import nexj.core.runtime.Instance;
import nexj.core.util.Undefined;

/**
 * Base implementation for table-based unit of work items.
 */
public abstract class SQLWork extends Work
{
   // constants
   
   /**
    * Work item type constants.
    * The value of the constant is used also for sorting the work items.
    */
   public final static int DELETE = 1;
   public final static int UPDATE = 2;
   public final static int INSERT = 3;

   // attributes
   
   /**
    * The column value array for the table.
    */
   protected Object[] m_valueArray;

   // associations

   /**
    * The table for which this work item applies.
    */
   protected Table m_table;
   
   /**
    * The mapping array for the table. Can be null.
    */
   protected RelationalPrimitiveMapping[] m_mappingArray;
   
   /**
    * The persistence adapter.
    */
   protected SQLAdapter m_adapter;
   
   // constructors

   /**
    * Constructs the SQLWork item.
    * @param instance The instance for which the work item applies.
    * @param table The table for which the work item applies.
    * @param adapter The persistence adapter.
    */
   protected SQLWork(Instance instance, Table table, SQLAdapter adapter)
   {
      super(instance);
      m_valueArray = new Object[table.getColumnCount()];
      Arrays.fill(m_valueArray, Undefined.VALUE);
      m_table = table;
      m_mappingArray = table.findMappingArray((RelationalMapping)m_mapping);
      m_adapter = adapter;
   }

   // operations

   /**
    * Sets the work item data.
    * @param instance The instance for which the work item applies.
    * @param table The table for which the work item applies.
    */
   protected void setData(Instance instance, Table table)
   {
      m_instance = instance;
      m_table = table;
   }

   /**
    * @return The table for which the work item applies.
    */
   public final Table getTable()
   {
      return m_table;
   }

   /**
    * @return True if this is a primary table work item.
    */
   public final boolean isPrimary()
   {
      return m_table == ((RelationalMapping)m_mapping).getPrimaryTable();
   }

   /**
    * @see nexj.core.persistence.Work#getAdapter()
    */
   public final PersistenceAdapter getAdapter()
   {
      return m_adapter;
   }

   /**
    * Sets the identity generator flag.
    * @param bIdentity The identity generator flag to set.
    */
   public void setIdentity(boolean bIdentity)
   {
      throw new UnsupportedOperationException();
   }

   /**
    * @return The identity generator flag.
    */
   public boolean isIdentity()
   {
      return false;
   }

   /**
    * @return True if the statement should be prepared as callable.
    */
   public boolean isCallable()
   {
      return false;
   }

   /**
    * @return True if a compound statement is generated.
    */
   public boolean isCompound()
   {
      return false;
   }

   /**
    * Sets the OID from the associated instance.
    */
   public final void setOID()
   {
      setKeyValue(m_table.getPrimaryKey(), m_table.getPrimaryKey(), m_instance);
   }

   /**
    * Computes the OID based on table column values.
    * @return The computed OID.
    */
   public final OID computeOID()
   {
      OID oldOID = m_instance.getOID(); 
      Index pk = m_table.getPrimaryKey();
      int nCount = pk.getIndexColumnCount();
      Object[] values = null;

      for (int i = 0; i < nCount; ++i)
      {
         Column column = pk.getIndexColumn(i).getColumn();
         Object value = m_valueArray[column.getOrdinal()];
         
         if (value == Undefined.VALUE)
         {
            if (oldOID == null)
            {
               return null;
            }
            
            value = oldOID.getValue(i);
         }

         if (value == null)
         {
            return null;
         }
         
         if (values == null)
         {
            values = new Object[nCount];
         }

         values[i] = value;
      }

      return new OID(values);
   }

   /**
    * Sets a table column value.
    * @param column The column which value to set.
    * @param value The value to set.
    */
   public void setValue(Column column, Object value)
   {
      m_valueArray[column.getOrdinal()] = value;
   }

   /**
    * Gets a table column value.
    * @param column The column which value to return.
    * @return The column value.
    */
   public final Object getValue(Column column)
   {
      return m_valueArray[column.getOrdinal()];
   }

   /**
    * Gets a table column value by column ordinal number.
    * @param nOrdinal The column ordinal number.
    * @return The column value.
    */
   public final Object getValue(int nOrdinal)
   {
      return m_valueArray[nOrdinal];
   }

   /**
    * Sets a table column value and updates the corresponding instance attribute value.
    * @param column The column which value to set.
    * @param value The value to set.
    */
   public final void setInstanceValue(Column column, Object value)
   {
      setValue(column, value);

      if (m_mappingArray != null)
      {
         RelationalPrimitiveMapping mapping = m_mappingArray[column.getOrdinal()];

         if (mapping != null)
         {
            Attribute attribute = mapping.getAttribute();

            m_instance.setValueDirect(attribute.getOrdinal(), ((Primitive)attribute.getType()).convert(value));
         }
      }
   }

   /**
    * @see nexj.core.persistence.Work#setKeyValue(nexj.core.meta.persistence.Key, nexj.core.meta.persistence.Key, nexj.core.runtime.Instance)
    */
   public final void setKeyValue(Key dstKey, Key srcKey, Instance instance)
   {
      Index index = (Index)dstKey;
      
      if (index == null)
      {
         return;
      }

      int nCount = index.getIndexColumnCount();

      if (instance == null)
      {
         for (int i = 0; i < nCount; ++i)
         {
            Column column = index.getIndexColumn(i).getColumn();

            if (!column.isPrimary() ||
                !(srcKey instanceof Index) ||
                ((Index)srcKey).getIndexColumn(i).getColumn() != column)
            {
               setInstanceValue(column, null);
            }
         }
      }
      else
      {
         OID oid = instance.getOID();

         if (srcKey.isObjectKey() && nCount != oid.getCount())
         {
            throw new PersistenceException("err.rpc.oidPartCount", 
               new Object[]{m_instance.getMetaclass().getName()});
         }

         for (int i = 0; i < nCount; ++i)
         {
            setInstanceValue(index.getIndexColumn(i).getColumn(), oid.getValue(srcKey.getObjectKeyPartOrdinal(i)));
         }
      }
   }

   /**
    * Sets up the locking attribute, if available.
    */
   public final void setupLocking()
   {
      RelationalMapping relMapping = (RelationalMapping)m_mapping;
      Attribute lockAttribute = relMapping.getLockingAttribute();

      if (lockAttribute != null)
      {
         Object value = m_instance.getValueDirect(lockAttribute.getOrdinal());

         if (value != Undefined.VALUE)
         {
            Column column = ((RelationalPrimitiveMapping)relMapping
               .getAttributeMapping(lockAttribute)).getColumn();
            
            value = column.getValueType().convert(value);
            setLockingColumn(column);
            setValue(column, value);
         }
      }
   }

   /**
    * Template method to set the locking column.
    * @param column The locking column to set.
    */
   protected void setLockingColumn(Column column)
   {
   }

   /**
    * @return The locking column, or null if none.
    */
   protected Column getLockingColumn()
   {
      return null;
   }

   /**
    * Finds a denormalized column source.
    * @param column The destination (denormalized) column.
    * @return The source column, or null if not found.
    */
   protected Column findSource(Column column)
   {
      if (column.isDenormalized())
      {
         RelationalMapping mapping = (RelationalMapping)m_mapping;

         for (int i = 0, n = mapping.getDenormCount(); i < n; ++i)
         {
            RelationalDenorm denorm = mapping.getDenorm(i);

            if (denorm.getTable() == m_table)
            {
               Column src = denorm.findSource(column);

               if (src != null)
               {
                  return src;
               }
            }
         }
      }

      return null;
   }

   /**
    * @return The SQL statement implementing this work item.
    */
   public abstract String getSQL();

   /**
    * Binds the values to the prepared statement.
    * @param stmt The prepared statement.
    * @param proto The prototype work item, on which getSQL has been invoked.
    */
   public abstract void bind(PreparedStatement stmt, SQLWork proto) throws SQLException;

   /**
    * @return True is the work item can be batched.
    */
   public abstract boolean isBatchable();

   /**
    * Executes the prepared statement.
    * @param stmt The prepared statement.
    * @param workArray The work item array.
    * @param nStart The index of the first work item in the array.
    * @param nEnd The index of the last work item in the array plus 1.
    */
   public abstract void execute(PreparedStatement stmt, Work[] workArray, int nStart, int nEnd) throws SQLException;

   /**
    * @return The estimate of the data length in bytes. 
    */
   public int getDataSize()
   {
      int nSize = 0;

      for (int i = 0, n = m_valueArray.length; i < n; ++i)
      {
         Object value = m_valueArray[i];

         if (value != Undefined.VALUE)
         {
            nSize += Primitive.getDataSize(value);

            if (nSize < 0)
            {
               nSize = Integer.MAX_VALUE;
            }
         }
      }

      return nSize;
   }

   /**
    * @see nexj.core.persistence.Work#compareTo(nexj.core.persistence.Work)
    */
   public final int compareTo(Work work)
   {
      SQLWork sqlWork = (SQLWork)work;

      int n = m_table.getName().compareTo(sqlWork.m_table.getName());

      if (n != 0)
      {
         return n;
      }

      n = ((isIdentity()) ? 1 : 0) - ((sqlWork.isIdentity()) ? 1 : 0);

      if (n != 0)
      {
         return n;
      }

      int nCount = m_valueArray.length;

      for (int i = 0; i < nCount; ++i)
      {
         n = ((m_valueArray[i] == Undefined.VALUE) ? 0 : 1) -
            ((sqlWork.m_valueArray[i] == Undefined.VALUE) ? 0 : 1);
         
         if (n != 0)
         {
            return n;
         }
      }

      return 0;
   }

   /**
    * @see nexj.core.persistence.Work#equals(nexj.core.persistence.Work)
    */
   public boolean equals(Work work)
   {
      return m_table == ((SQLWork)work).m_table;
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return m_instance.hashCode() ^ m_table.hashCode();
   }
}
