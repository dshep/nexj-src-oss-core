// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;


/**
 * Denormalization metadata for an attribute referencing an object.
 */
public final class RelationalClassDenorm extends RelationalDenorm
{
   // associations

   /**
    * The table index specifying the denormalized key.
    */
   private Index m_sourceKey;

   // constructors

   /**
    * Constructs the denorm.
    * @param The denorm mapping.
    */
   public RelationalClassDenorm(RelationalClassMapping mapping)
   {
      super(mapping);
   }

   // operations

   /**
    * Sets the denormalized association key of the source class.
    * @param sourceKey The the association key of the source class to set.
    */
   public void setSourceKey(Index sourceKey)
   {
      verifyNotReadOnly();
      m_sourceKey = sourceKey;

      if (sourceKey != null)
      {
         sourceKey.setMapped();

         boolean bRequired = sourceKey.isObjectKeyPart() || m_mapping.getAttribute().isRequired();

         for (int i = 0; i < sourceKey.getIndexColumnCount(); ++i)
         {
            Column column = sourceKey.getIndexColumn(i).getColumn();

            column.setRequired(bRequired, false);
            column.setDenormalized(true);
         }
      }
   }

   /**
    * @return The association key of the source class.
    */
   public Index getSourceKey()
   {
      return m_sourceKey;
   }

   /**
    * @see nexj.core.meta.persistence.sql.RelationalDenorm#getTable()
    */
   public Table getTable()
   {
      return m_sourceKey.getTable();
   }

   /**
    * @see nexj.core.meta.persistence.sql.RelationalDenorm#findSource(nexj.core.meta.persistence.sql.Column)
    */
   public Column findSource(Column column)
   {
      IndexColumn indexColumn = m_sourceKey.findIndexColumn(column);

      return (indexColumn != null) ? ((RelationalClassMapping)m_mapping).getSourceKey()
         .getIndexColumn(indexColumn.getOrdinal()).getColumn() : null;
   }

   /**
    * @see java.lang.Object#equals(java.lang.Object)
    */
   public boolean equals(Object obj)
   {
      if (obj instanceof RelationalClassDenorm)
      {
         RelationalClassDenorm denorm = (RelationalClassDenorm)obj;

         return m_mapping == denorm.m_mapping && m_sourceKey == denorm.m_sourceKey;
      }

      return false;
   }
}
