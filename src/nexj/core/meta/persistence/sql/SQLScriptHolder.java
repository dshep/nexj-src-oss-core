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
 * SQL script holder.
 */
public abstract class SQLScriptHolder extends MetadataObject
{
   // associations

   /**
    * The SQL script collection.
    */
   protected List m_scriptList = new ArrayList(2); // of type SQLScript

   // operations

   /**
    * Adds a new SQL script to the step.
    * @param script The SQL script to add.
    */
   public void addScript(SQLScript script)
   {
      verifyNotReadOnly();
      m_scriptList.add(script);
   }

   /**
    * Gets a SQL script by ordinal number.
    * @param nOrdinal The SQL script ordinal number (0-based).
    * @return The SQL script object.
    */
   public SQLScript getScript(int nOrdinal)
   {
      return (SQLScript) m_scriptList.get(nOrdinal);
   }

   /**
    * @return The SQL script count.
    */
   public int getScriptCount()
   {
      return m_scriptList.size();
   }

   /**
    * @return An iterator for the contained SQL script objects.
    */
   public Iterator getScriptIterator()
   {
      return m_scriptList.iterator();
   }

   /**
    * Validates the SQL scripts.
    * @param schema The schema against which to validate.
    * @param table The context table. Can be null.
    * @param adapterArray An array of adapters that should be validated (null == validate all).
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(
      RelationalSchema schema, Table table, DataSourceAdapter[] adapterArray)
      throws MetadataException
   {
      MetadataCompoundValidationException eh = null;

      // validate every script
      for (int i = 0, nCount = getScriptCount(); i < nCount; ++i)
      {
         try
         {
            getScript(i).validate(schema, table, adapterArray);
         }
         catch (UncheckedException e)
         {
            MetadataValidationException x = new MetadataValidationException(e);

            x.setProperty("sql", String.valueOf(i));
            setProperties(x);
            eh = addException(eh, x);
         }
      }

      if (eh != null)
      {
         throw eh;
      }
   }

   /**
    * Template method to set validation exception properties.
    */
   protected abstract void setProperties(MetadataValidationException e);
}
