// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.Attribute;
import nexj.core.meta.Primitive;
import nexj.core.meta.persistence.AttributeMapping;
import nexj.core.util.ObjUtil;

/**
 * Relational mapping of an attribute containing a primitive value.
 */
public final class RelationalPrimitiveMapping extends AttributeMapping
{
   // associations

   /**
    * The table column to which the attribute is mapped.
    */
   private Column m_column;

   // operations
   
   /**
    * @see nexj.core.meta.persistence.AttributeMapping#setAttribute(nexj.core.meta.Attribute)
    */
   public void setAttribute(Attribute attribute)
   {
      super.setAttribute(attribute);
      
      if (attribute != null)
      {
         m_column.addAttribute(attribute);

         for (int i = 0, n = getDenormCount(); i < n; ++i)
         {
            ((RelationalPrimitiveDenorm)getDenorm(i)).getColumn().addAttribute(attribute);
         }
      }
   }

   /**
    * Sets the target table column.
    * @param column The target table column to set.
    */
   public void setColumn(Column column)
   {
      verifyNotReadOnly();
      m_column = column;
   }

   /**
    * @return The target table column.
    */
   public Column getColumn()
   {
      return m_column;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isMultiplexed()
    */
   public boolean isMultiplexed()
   {
      return m_column.isMultiplexed();
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#isAliasOf(nexj.core.meta.persistence.AttributeMapping)
    */
   public boolean isAliasOf(AttributeMapping mapping)
   {
      return mapping instanceof RelationalPrimitiveMapping &&
         ((RelationalPrimitiveMapping)mapping).m_column == m_column;
   }

   /**
    * @see nexj.core.meta.persistence.AttributeMapping#getMaxLength(Primitive)
    */
   public int getMaxLength(Primitive type)
   {
      if (m_column.getType() == Primitive.STRING)
      {
         if (type == Primitive.BINARY)
         {
            return m_column.getPrecision() / 2;
         }
         
         return m_column.getPrecision();
      }
      
      if (m_column.getType() == Primitive.BINARY)
      {
         if (type == Primitive.STRING)
         {
            if (m_column.getPrecision() <= Integer.MAX_VALUE / 2)
            {
               return m_column.getPrecision() * 2;
            }
            
            return Integer.MAX_VALUE;
         }
         
         return m_column.getPrecision();
      }

      return 0;
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return ObjUtil.getShortClassName(this) + " for " + getAttribute() + " to " + getColumn();
   }
}
