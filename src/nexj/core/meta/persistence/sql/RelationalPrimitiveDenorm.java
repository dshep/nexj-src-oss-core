// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;


/**
 * Primitive attribute denormalization metadata.
 */
public final class RelationalPrimitiveDenorm extends RelationalDenorm
{
   // attributes

   /**
    * The column where the primitive value is denormalized.
    */
   protected Column m_column;

   // constructors

   /**
    * Constructs the denorm.
    * @param mapping The attribute mapping.
    */
   public RelationalPrimitiveDenorm(RelationalPrimitiveMapping mapping)
   {
      super(mapping);
   }

   // operations

   /**
    * Sets the column where the primitive value is denormalized.
    * @param column The column where the primitive value is denormalized to set.
    */
   public void setColumn(Column column)
   {
      verifyNotReadOnly();
      m_column = column;

      if (column != null)
      {
         column.setDenormalized(true);
      }
   }

   /**
    * @return The column where the primitive value is denormalized.
    */
   public Column getColumn()
   {
      return m_column;
   }

   /**
    * @see nexj.core.meta.persistence.sql.RelationalDenorm#getTable()
    */
   public Table getTable()
   {
      return m_column.getTable();
   }

   /**
    * @see nexj.core.meta.persistence.sql.RelationalDenorm#findSource(nexj.core.meta.persistence.sql.Column)
    */
   public Column findSource(Column column)
   {
      return (m_column == column) ? ((RelationalPrimitiveMapping)m_mapping).getColumn() : null;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof RelationalPrimitiveDenorm)
      {
         RelationalPrimitiveDenorm denorm = (RelationalPrimitiveDenorm)obj;

         return m_mapping == denorm.m_mapping && m_column == denorm.m_column;
      }

      return false;
   }
}
