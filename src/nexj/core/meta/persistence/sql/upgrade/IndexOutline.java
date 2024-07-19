// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import java.util.ArrayList;
import java.util.List;

import nexj.core.meta.NamedMetadataObject;
import nexj.core.meta.persistence.sql.Index;
import nexj.core.meta.persistence.sql.IndexColumn;

/**
 * Index outline, storing the index attributes and the column names.
 */
public class IndexOutline extends NamedMetadataObject
{
   // attributes

   /**
    * The index type (BTREE, VIRTUAL, ...).
    */
   protected byte m_nType = Index.BTREE;

   /**
    * The index page fill factor percent. 0 for the database type default. -1 for the schema default.
    */
   protected int m_nFill = -1;

   /**
    * The unique constraint flag.
    * True if this is a unique constraint.
    */
   protected boolean m_bUnique;

   /**
    * The related table name.
    */
   protected String m_sRelatedTableName;
   
   // associations
   
   /**
    * Column name and ascending flag list: String[2*n}, Boolean[2*n+1].
    */
   protected List m_columnList = new ArrayList(4);

   /**
    * The aspect override list: sName[2*n], bInclusive[2*n+1].
    */
   protected List m_aspectOverrideList;

   // operations
   
   /**
    * Sets the index type (BTREE, VIRTUAL, ...).
    * @param nType The index type (BTREE, VIRTUAL, ...) to set.
    */
   public void setType(byte nType)
   {
      verifyNotReadOnly();
      m_nType = nType;
   }

   /**
    * @return The index type (BTREE, VIRTUAL, ...).
    */
   public byte getType()
   {
      return m_nType;
   }
   
   /**
    * Sets the unique constraint flag.
    * @param bUnique The unique constraint flag to set.
    */
   public void setUnique(boolean bUnique)
   {
      verifyNotReadOnly();
      m_bUnique = bUnique;
   }

   /**
    * @return The unique constraint flag. True if the constraint is unique.
    */
   public boolean isUnique()
   {
      return m_bUnique;
   }

   /**
    * Sets the index page fill factor percent.
    * @param nFill The default index page fill factor percent to set.
    */
   public void setFill(int nFill)
   {
      verifyNotReadOnly();
      m_nFill = nFill;
   }

   /**
    * @return The index page fill factor percent.
    */
   public int getFill()
   {
      return m_nFill;
   }

   /**
    * Sets the related table name.
    * @param sRelatedTableName The related table name to set.
    */
   public void setRelatedTableName(String sRelatedTableName)
   {
      verifyNotReadOnly();
      m_sRelatedTableName = sRelatedTableName;
   }

   /**
    * @return The related table name.
    */
   public String getRelatedTableName()
   {
      return m_sRelatedTableName;
   }

   /**
    * Adds a column name to the index outline.
    * @param sName The column name.
    * @param bAscending The ascending sort order flag.
    */
   public void addColumn(String sName, boolean bAscending)
   {
      m_columnList.add(sName);
      m_columnList.add(Boolean.valueOf(bAscending));
   }

   /**
    * Gets the name of the column with a given ordinal number.
    * @param nOrdinal The column ordinal number.
    * @return The column name. 
    */
   public String getColumnName(int nOrdinal)
   {
      return (String)m_columnList.get(nOrdinal << 1);
   }
   
   /**
    * Get the column ascending sort order flag.
    * @param nOrdinal The column ordinal number.
    * @return The ascending sort order flag.
    */
   public boolean isColumnAscending(int nOrdinal)
   {
      return ((Boolean)m_columnList.get((nOrdinal << 1) + 1)).booleanValue();
   }

   /**
    * @return The index column count.
    */
   public int getColumnCount()
   {
      return m_columnList.size() >> 1;
   }

   /**
    * Adds an aspect override to the step.
    * @param sName The aspect name.
    * @param bInclusive True if the aspect is inclusive.
    */
   public void addAspectOverride(String sName, boolean bInclusive)
   {
      verifyNotReadOnly();

      if (m_aspectOverrideList == null)
      {
         m_aspectOverrideList = new ArrayList(2);
      }

      m_aspectOverrideList.add(sName);
      m_aspectOverrideList.add(Boolean.valueOf(bInclusive));
   }

   /**
    * Copies the index outline to an index.
    * @param index The destination index.
    */
   public void copyTo(Index index)
   {
      index.setUnique(m_bUnique);
      index.setType(m_nType);
      index.setFill(m_nFill);
      
      if (m_sRelatedTableName != null)
      {
         index.setRelatedTable(index.getTable().getSchema().getTable(m_sRelatedTableName));
      }

      for (int i = 0, n = getColumnCount(); i < n; ++i)
      {
         index.addIndexColumn(new IndexColumn(
            index.getTable().getColumn(getColumnName(i)),
            isColumnAscending(i)));
      }

      if (m_aspectOverrideList != null)
      {
         for (int i = 0, n = m_aspectOverrideList.size(); i < n; i += 2)
         {
            index.addAspectOverride(
               index.getTable().getSchema().getIndex((String)m_aspectOverrideList.get(i)),
               ((Boolean)m_aspectOverrideList.get(i + 1)).booleanValue());
         }
      }
   }
}
