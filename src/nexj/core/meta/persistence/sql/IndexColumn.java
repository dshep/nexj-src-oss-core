// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.MetadataObject;

/**
 * Index column descriptor.
 */
public class IndexColumn extends MetadataObject implements Cloneable
{
   // attributes

   /**
    * The column ordinal number.
    */
   private int m_nOrdinal = -1;

   /**
    * The primary key part ordinal number.
    */
   private int m_nPrimaryKeyPartOrdinal = -1;

   /**
    * The ascending sort order flag.
    * True if the column is sorted in ascending order.
    */
   private boolean m_bAscending;

   // associations

   /**
    * The referenced table column.
    */
   private Column m_column;

   /**
    * The containing index.
    */
   private Index m_index;

   // constructors
   
   /**
    * Creates an index column object.
    * @param column The referenced column.
    * @param bAscending The ascending sort order flag.
    */
   public IndexColumn(Column column, boolean bAscending)
   {
      m_column = column;
      m_bAscending = bAscending;
   }

   // operations
   
   /**
    * Sets the column ordinal number.
    * @param nOrdinal The column ordinal number to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The column ordinal number.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets the primary key part ordinal number.
    * @param nPrimaryKeyPartOrdinal The primary key part ordinal number to set.
    */
   public void setPrimaryKeyPartOrdinal(int nPrimaryKeyPartOrdinal)
   {
      verifyNotReadOnly();
      m_nPrimaryKeyPartOrdinal = nPrimaryKeyPartOrdinal;
   }

   /**
    * @return The primary key part ordinal number.
    */
   public int getPrimaryKeyPartOrdinal()
   {
      return m_nPrimaryKeyPartOrdinal;
   }
   
   /**
    * Sets the ascending sort order flag.
    * @param bAscending The ascending sort order flag to set.
    */
   public void setAscending(boolean bAscending)
   {
      verifyNotReadOnly();
      m_bAscending = bAscending;
   }

   /**
    * @return The ascending sort order flag.
    */
   public boolean isAscending()
   {
      return m_bAscending;
   }
   
   /**
    * Sets the containing index.
    * @param index The containing index to set.
    */
   public void setIndex(Index index)
   {
      verifyNotReadOnly();
      m_index = index;
   }

   /**
    * @return The containing index.
    */
   public Index getIndex()
   {
      return m_index;
   }
   
   /**
    * Sets the referenced table column.
    * @param column The referenced table column to set.
    */
   public void setColumn(Column column)
   {
      verifyNotReadOnly();
      m_column = column;
   }

   /**
    * @return The referenced table column.
    */
   public Column getColumn()
   {
      return m_column;
   }

   /**
    * Clones the index column.
    * @param index The new index instance.
    * @see nexj.core.meta.MetadataObject#clone()
    */
   protected IndexColumn clone(Index index)
   {
      IndexColumn indexColumn = (IndexColumn)super.clone();

      indexColumn.m_index = index;
      indexColumn.m_column = (Column)index.m_table.m_columnMap.get(m_column.getName());

      return indexColumn;
   }
}
