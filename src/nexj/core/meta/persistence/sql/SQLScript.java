// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nexj.core.meta.MetadataCompoundValidationException;
import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.MetadataValidationException;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.UncheckedException;

/**
 * SQL script container that switches the SQL script based on the adapter.
 */
public class SQLScript extends MetadataObject
{
   // associations

   /**
    * The SQL statement collection.
    */
   protected List m_statementList = new ArrayList(2); // of type SQLStatement

   // operations
   
   /**
    * Adds a new SQL statement to the script.
    * @param statement The SQL statement to add.
    */
   public void addStatement(SQLStatement statement)
   {
      verifyNotReadOnly();
      m_statementList.add(statement);
      statement.setScript(this);
   }

   /**
    * Gets a SQL statement by ordinal number.
    * @param nOrdinal The SQL statement ordinal number (0-based).
    * @return The SQL statement object.
    */
   public SQLStatement getStatement(int nOrdinal)
   {
      return (SQLStatement)m_statementList.get(nOrdinal);
   }

   /**
    * Finds the SQL statement to execute based on the persistence adapter.
    * @param adapter The persistence adapter.
    * @return The SQL statement, or null if none.
    */
   public SQLStatement findStatement(DataSourceAdapter adapter)
   {
      for (int i = 0, n = m_statementList.size(); i < n; ++i)
      {
         SQLStatement stmt = (SQLStatement)m_statementList.get(i);

         if (stmt.isCompatible(adapter))
         {
            return stmt;
         }
      }

      return null;
   }

   /**
    * @return The SQL statement count.
    */
   public int getStatementCount()
   {
      return m_statementList.size();
   }

   /**
    * @return An iterator for the contained SQL statement objects.
    */
   public Iterator getStatementIterator()
   {
      return m_statementList.iterator();
   }

   /**
    * Finds the SQL statement to execute based on the persistence adapter.
    * @param adapter The persistence adapter.
    * @return The SQL statement, or null if none.
    */
   public String findSQL(DataSourceAdapter adapter)
   {
      SQLStatement stmt = findStatement(adapter);

      return (stmt == null) ? null : stmt.getSQL();
   }

   /**
    * Finds the SQL statement to execute based on the persistence adapter.
    * @param adapter The persistence adapter.
    * @return The SQL statement. Can be null.
    * @throws MetadataException if the adapter does not match.
    */
   public String getSQL(DataSourceAdapter adapter) throws MetadataException
   {
      SQLStatement stmt = findStatement(adapter);

      if (stmt == null)
      {
         throw new MetadataException("err.meta.sql.statementAdapter", new Object[]{adapter.getName()});
      }

      return stmt.getSQL();
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      ((ArrayList)m_statementList).trimToSize(); // free unused memory
   }

   /**
    * Validates the SQL script.
    * @param schema The schema against which to validate.
    * @param table The context table. Can be null.
    * @param adapterArray An array of adapters that should be validated (null == schema's adapter).
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(
      RelationalSchema schema, Table table, DataSourceAdapter[] adapterArray)
      throws MetadataException
   {
      MetadataCompoundValidationException eh = null;

      // validate every statement
      for (int i = 0, nCount = getStatementCount(); i < nCount; ++i)
      {
         try
         {
            getStatement(i).validate(schema, table);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);

            x.setProperty("sql", String.valueOf(i));
            setProperties(x);
            eh = addException(eh, x);
         }
      }

      // ensure there is a statement for every requested adapter
      if (adapterArray == null)
      {
         DataSourceAdapter adapter = schema.getDataSource().getAdapter();

         if (adapter != null)
         {
            try
            {
               getSQL(adapter);
            }
            catch (UncheckedException e)
            {
               eh = addException(eh, e);
            }
         }
      }
      else
      {
         for (int i = 0, nCount = adapterArray.length; i < nCount; ++i)
         {
            try
            {
               getSQL(adapterArray[i]);
            }
            catch (UncheckedException e)
            {
               eh = addException(eh, e);
            }
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      int nHash = 0;

      for (int i = 0, n = getStatementCount(); i < n; ++i)
      {
         nHash ^= getStatement(i).hashCode();
      }

      return nHash;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof SQLScript))
      {
         return false;
      }

      if (this == obj)
      {
         return true; // skip all the comparison logic if same object
      }

      SQLScript other = (SQLScript)obj;
      int nStmtCount = getStatementCount();

      if (nStmtCount != other.getStatementCount())
      {
         return false; // not same number of patterns
      }

      for (int nStmt = 0; nStmt < nStmtCount; ++nStmt)
      {
         if (!getStatement(nStmt).equals(other.getStatement(nStmt)))
         {
            return false;
         }
      }

      return true;
   }
}
