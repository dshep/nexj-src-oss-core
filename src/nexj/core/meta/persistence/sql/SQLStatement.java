// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import nexj.core.meta.MetadataException;
import nexj.core.meta.MetadataObject;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.DataSourceAdapter;
import nexj.core.meta.persistence.DataSourceType;
import nexj.core.util.Binary;
import nexj.core.util.ExceptionHolder;
import nexj.core.util.ObjUtil;
import nexj.core.util.StringUtil;
import nexj.core.util.SubstReader;

/**
 * SQL statement used in the SQLExec step.
 */
public class SQLStatement extends MetadataObject
{
   // attributes

   /**
    * The SQL statement.
    */
   protected String m_sSQL;
   
   // associations

   /**
    * List of compatible adapter name patterns: Pattern[n*2], String[n*2+1].
    */  
   protected List m_adapterList;

   /**
    * The SQL script.
    */
   protected SQLScript m_script;

   // operations

   /**
    * Sets the containing SQL script.
    * @param script The SQL script to set.
    */
   public void setScript(SQLScript script)
   {
      verifyNotReadOnly();
      m_script = script;
   }

   /**
    * @return The SQL script.
    */
   public SQLScript getScript()
   {
      return m_script;
   }

   /**
    * Sets the SQL statement.
    * @param sSQL The SQL statement to set.
    */
   public void setSQL(String sSQL)
   {
      verifyNotReadOnly();
      m_sSQL = sSQL;
   }

   /**
    * @return The SQL statement.
    */
   public String getSQL()
   {
      return m_sSQL;
   }

   /**
    * Adds a compatible persistence adapter name pattern.
    * @param sPattern The adapter name pattern, with * and ? wildcards.
    * @param type The data source type.
    */
   public void addAdapter(String sPattern, DataSourceType type)
   {
      verifyNotReadOnly();

      if (m_adapterList == null)
      {
         m_adapterList = new ArrayList(4);
      }

      Pattern pattern = Primitive.likePattern(sPattern, 0);
      boolean bFound = false;

      for (Iterator itr = type.getAdapterIterator(); itr.hasNext();)
      {
         if (pattern.matcher(((DataSourceAdapter)itr.next()).getName()).matches())
         {
            bFound = true;
            break;
         }
      }

      if (!bFound)
      {
         throw new MetadataException("err.meta.persistence.sql.unknownAdapter", new Object[]{sPattern});
      }
      
      m_adapterList.add(pattern);
      m_adapterList.add(sPattern);
   }

   /**
    * Gets an adapter pattern by ordinal number.
    * @param nOrdinal The adapter ordninal number.
    * @return The adapter name pattern, with * and ? wildcards.
    */
   public String getAdapterPattern(int nOrdinal)
   {
      return (String)m_adapterList.get((nOrdinal << 1) + 1);
   }

   /**
    * @return The adapter count.
    */
   public int getAdapterCount()
   {
      return (m_adapterList == null) ? 0 : m_adapterList.size() >> 1;
   }

   /**
    * Determines whether this statement is compatible with a given adapter.
    * @param adapter The persistence adapter to test against. 
    */
   public boolean isCompatible(DataSourceAdapter adapter)
   {
      if (m_adapterList == null)
      {
         return true;
      }

      for (int i = 0, n = m_adapterList.size(); i < n; i += 2)
      {
         if (((Pattern)m_adapterList.get(i)).matcher(adapter.getName()).matches())
         {
            return true;
         }
      }

      return false;
   }

   /**
    * Validates the SQL statement.
    * @param schema The schema against which to validate.
    * @param table The context table. Can be null.
    * @see nexj.core.meta.MetadataObject#validate(nexj.core.meta.ContextMetadata, ExceptionHolder)
    */
   public void validate(RelationalSchema schema, Table table) throws MetadataException
   {
      if (m_sSQL != null)
      {
         SubstReader reader = new SQLSubstReader(new StringReader(m_sSQL), schema, table)
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

            protected String getQuotedTableName(Table table)
            {
               return table.getTableName();
            }

            protected String getQuotedIndexName(Index index)
            {
               return index.getName();
            }

            protected String getQuotedObjectName(SQLObject object)
            {
               return object.getObjectName();
            }

            protected String getQuotedKeyword(String sName)
            {
               return sName;
            }

            protected String getQuotedOwnerName()
            {
               return null;
            }

            protected String getQuotedRoleName()
            {
               return null;
            }

            protected boolean isColumnCaseInsensitive(Column column)
            {
               return false;
            }

            protected String getBinaryLiteral(String sHex)
            {
               Binary.parse(sHex);

               return null;
            }

            protected String getGUID()
            {
               return null;
            }

            protected String getNow()
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
            while (reader.read() >= 0) ;
         }
         catch (IOException e)
         {
            ObjUtil.rethrow(e);
         }
      }
   }

   /**
    * @see java.lang.Object#hashCode()
    */
   public int hashCode()
   {
      return ((m_sSQL == null) ? 0 : m_sSQL.hashCode()) ^ getAdapterCount();
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (!(obj instanceof SQLStatement))
      {
         return false;
      }

      SQLStatement stmt = (SQLStatement)obj;
      int nAdapterCount = getAdapterCount();

      if (nAdapterCount != stmt.getAdapterCount() ||
         !ObjUtil.equal(StringUtil.trimToNull(m_sSQL), StringUtil.trimToNull(stmt.m_sSQL)))
      {
         return false;
      }

      for (int nAdapter = 0; nAdapter < nAdapterCount; ++nAdapter)
      {
         String sPattern = getAdapterPattern(nAdapter);
         int nOtherAdapter;

         for (nOtherAdapter = 0; nOtherAdapter < nAdapterCount; ++nOtherAdapter)
         {
            if (ObjUtil.equal(sPattern, stmt.getAdapterPattern(nOtherAdapter)))
            {
               break;
            }
         }

         if (nOtherAdapter == nAdapterCount)
         {
            return false; // pattern not found (allow for pattern reordering
         }
      }

      return true;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      StringBuilder buf = new StringBuilder(128);

      buf.append("SQLStatement(adapters=");

      if (m_adapterList == null)
      {
         buf.append("[*]");
      }
      else
      {
         buf.append(m_adapterList);
      }

      buf.append(", sql=");
      buf.append(m_sSQL);
      buf.append(')');

      return buf.toString();
   }
}
