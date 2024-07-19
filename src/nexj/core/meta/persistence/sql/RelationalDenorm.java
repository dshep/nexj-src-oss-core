// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.persistence.AttributeDenorm;
import nexj.core.meta.persistence.AttributeMapping;

/**
 * Relational attribute denorm.
 */
public abstract class RelationalDenorm extends AttributeDenorm
{
   // constructors

   /**
    * Constructs the denorm.
    * @param mapping The attribute mapping.
    */
   protected RelationalDenorm(AttributeMapping mapping)
   {
      super(mapping);
   }

   // operations

   /**
    * @return The destination table.
    */
   public abstract Table getTable();

   /**
    * Finds the source of the denormalized column.
    * @param column The destination column.
    * @return The source column, or null if not found.
    */
   public abstract Column findSource(Column column);
}
