// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql.upgrade;

import nexj.core.meta.persistence.sql.Column;
import nexj.core.meta.persistence.sql.ColumnBase;

/**
 * Column outline, storing column attributes.
 */
public class ColumnOutline extends ColumnBase
{
   // attributes

   /**
    * The nullable flag.
    */
   protected Boolean m_nullable;

   /**
    * The case insensitive flag.
    */
   protected Boolean m_caseInsensitive;

   /**
    * The literal binding flag.
    */
   protected Boolean m_literal;
   
   /**
    * The converter component name.
    */
   protected String m_sConverterName;

   // constructors

   /**
    * Constructs the outline.
    * @param sName The column name.
    */
   public ColumnOutline(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the outline.
    */
   public ColumnOutline()
   {
   }

   // operations

   /**
    * Sets the nullable flag.
    * @param nullable The nullable flag to set.
    */
   public void setNullable(Boolean nullable)
   {
      verifyNotReadOnly();
      m_nullable = nullable;
   }

   /**
    * @return The nullable flag.
    */
   public Boolean getNullable()
   {
      return m_nullable;
   }

   /**
    * Sets the case insensitive flag.
    * @param caseInsensitive The case insensitive flag to set.
    */
   public void setCaseInsensitive(Boolean caseInsensitive)
   {
      verifyNotReadOnly();
      m_caseInsensitive = caseInsensitive;
   }

   /**
    * @return The case insensitive flag.
    */
   public Boolean getCaseInsensitive()
   {
      return m_caseInsensitive;
   }

   /**
    * Sets the literal binding flag.
    * @param literal The literal binding flag to set.
    */
   public void setLiteral(Boolean literal)
   {
      verifyNotReadOnly();
      m_literal = literal;
   }

   /**
    * @return The literal binding flag.
    */
   public Boolean getLiteral()
   {
      return m_literal;
   }

   /**
    * Sets the converter component name.
    * @param sConverterName The converter component name to set.
    */
   public void setConverterName(String sConverterName)
   {
      verifyNotReadOnly();
      m_sConverterName = sConverterName;
   }

   /**
    * @return The converter component name.
    */
   public String getConverterName()
   {
      return m_sConverterName;
   }

   /**
    * Copies the outline attributes to a column.
    * @param column The destination column.
    */
   public void copyTo(Column column)
   {
      if (m_sDescription != null)
      {
         column.setDescription(m_sDescription);
      }

      if (m_nullable != null)
      {
         column.setNullable(m_nullable.booleanValue());
      }

      if (m_caseInsensitive != null)
      {
         column.setCaseInsensitive(m_caseInsensitive.booleanValue());
      }

      if (m_literal != null)
      {
         column.setLiteral(m_literal.booleanValue());
      }

      if (m_type != null)
      {
         column.setType(m_type);
         column.setPrecision(m_nPrecision);
         column.setScale(m_nScale);
         column.setAllocation(m_nAllocation);
      }

      if (m_sConverterName != null)
      {
         column.setConverter((m_sConverterName.trim().length() == 0) ? null : 
            column.getTable().getSchema().getMetadata().getComponent(m_sConverterName));
      }
   }
}
