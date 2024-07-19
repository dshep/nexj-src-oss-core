// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import java.io.IOException;
import java.util.Iterator;

import nexj.core.meta.Attribute;
import nexj.core.meta.Component;
import nexj.core.meta.Metaclass;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;
import nexj.core.persistence.CalendarFactory;
import nexj.core.persistence.Converter;
import nexj.core.persistence.sql.SQLConverter;
import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.StringUtil;

/**
 * A table column.
 */
public class Column extends ColumnBase implements Cloneable, Printable
{
   // constants

   /**
    * Maximum portable column name length.
    */
   public final static int MAX_NAME_LENGTH = 29;
   
   /**
    * Maximum portable nvarchar column precision.
    */
   public final static int MAX_NVARCHAR_PRECISION = 2000;

   /**
    * Maximum portable varbinary column precision.
    */
   public final static int MAX_VARBINARY_PRECISION = 2000;
   
   // attributes
   
   /**
    * The column ordinal number in the table.
    */
   protected int m_nOrdinal = -1;

   /**
    * The primary key part flag.
    */
   protected boolean m_bPrimary;
   
   /**
    * True if the column is shared between two or more classes from different hierarchies.
    */
   protected boolean m_bMultiplexed;

   /**
    * The nullable flag.
    */
   protected boolean m_bNullable = true;
   
   /**
    * The required flag, computed from the attributes.
    */
   protected boolean m_bRequired;
   
   /**
    * True if the required flag has been initialized.
    */
   protected boolean m_bRequiredInitialized;

   /**
    * The case insensitive flag.
    */
   protected boolean m_bCaseInsensitive = true;
   
   /**
    * The literal binding flag. Set to true to disable parameter binding
    * on this column and use literal binding instead.
    */
   protected boolean m_bLiteral;

   /**
    * The denormalization destination flag.
    */
   protected boolean m_bDenormalized;

   /**
    * The time zone conversion flag.
    */
   protected boolean m_bTimeZoned;

   /**
    * True if the value cannot be converted by the database.
    */
   protected boolean m_bPostConverted;

   // associations

   /**
    * The containing table.
    */
   protected Table m_table;

   /**
    * The column value type. This can be different from
    * the column type if there is a converter.
    */
   protected Primitive m_valueType;

   /**
    * The type conversion component.
    */
   protected Component m_converter;
   
   /**
    * The last mapped metaclass (for multiplexing detection).
    */
   protected Metaclass m_lastMetaclass;

   // constructors
   
   /**
    * Creates a column with a given name.
    * @param sName The column name.
    * @param table The table.
    */
   public Column(String sName, Table table)
   {
      m_table = table;
      setName(sName);
   } 

   // operations

   /**
    * @see nexj.core.meta.NamedMetadataObject#setName(java.lang.String)
    */
   public void setName(String sName)
   {
      sName = StringUtil.intern(sName); // make sure always use interned value from setName()

      if (sName != null && sName.length() > MAX_NAME_LENGTH)
      {
         if (m_table == null || m_table.getSchema() == null || m_table.getSchema().isPortable())
         {
            throw new MetadataException("err.meta.columnNameLength",
               new Object[]{sName, Primitive.createInteger(MAX_NAME_LENGTH)});
         }
      }

      if (m_sName != null)
      {
         if (m_nOrdinal >= 0 && m_table != null)
         {
            if (sName != null)
            {
               Column column = m_table.findColumn(sName);

               if (column != null && column != this) 
               {
                  throw new MetadataException("err.meta.columnDup", new Object[]{sName, m_table.getName()});
               }
            }

            if (m_table.m_columnMap.remove(m_sName) != null && sName != null)
            {
               m_table.m_columnMap.put(sName, this);
            }
         }
      }

      super.setName(sName);
   }
   
   /**
    * Sets the column ordinal number in the table.
    * @param nOrdinal The column ordinal number in the table to set.
    */
   public void setOrdinal(int nOrdinal)
   {
      verifyNotReadOnly();
      m_nOrdinal = nOrdinal;
   }

   /**
    * @return The column ordinal number in the table.
    */
   public int getOrdinal()
   {
      return m_nOrdinal;
   }

   /**
    * Sets the decimal precision or text length.
    * @param nPrecision The decimal precision or text length to set.
    */
   public void setPrecision(int nPrecision)
   {
      super.setPrecision(nPrecision);

      if (nPrecision <= 0 || nPrecision > MAX_NVARCHAR_PRECISION)
      {
         m_bCaseInsensitive = false;
      }
   }

   /**
    * Sets the allocation type - FIXED, VARYING or LOB.
    * @param nAllocation The allocation type - FIXED, VARYING or LOCATOR to set.
    */
   public void setAllocation(byte nAllocation)
   {
      super.setAllocation(nAllocation);

      if (nAllocation == LOCATOR)
      {
         m_bCaseInsensitive = false;
      }
   }

   /**
    * Sets the primary key part flag.
    * @param bPrimary The primary key part flag to set.
    */
   public void setPrimary(boolean bPrimary)
   {
      verifyNotReadOnly();
      m_bPrimary = bPrimary;

      if (bPrimary)
      {
         setCaseInsensitive(false);
         setRequired(true, false);
      }
   }

   /**
    * @return The primary key part flag.
    */
   public boolean isPrimary()
   {
      return m_bPrimary;
   }

   /**
    * Sets the nullable flag.
    * @param bNullable The nullable flag to set.
    */
   public void setNullable(boolean bNullable)
   {
      verifyNotReadOnly();
      m_bNullable = bNullable;
   }

   /**
    * @return The nullable flag.
    */
   public boolean isNullable()
   {
      return m_bNullable && !m_bRequired;
   }
   
   /**
    * Sets the case insensitive flag.
    * @param bCaseInsensitive The case insensitive flag to set.
    */
   public void setCaseInsensitive(boolean bCaseInsensitive)
   {
      verifyNotReadOnly();
      m_bCaseInsensitive = bCaseInsensitive;
   }

   /**
    * @return The case insensitive flag.
    */
   public boolean isCaseInsensitive()
   {
      return m_bCaseInsensitive;
   }

   /**
    * Sets the literal binding flag.
    * @param bLiteral True to disable parameter binding on this column and
    * use literal binding instead.
    */
   public void setLiteral(boolean bLiteral)
   {
      verifyNotReadOnly();
      m_bLiteral = bLiteral;
   }

   /**
    * Gets the literal binding flag.
    * 
    * @return The literal binding flag; true to disable parameter binding on
    * this column and use literal binding instead.
    */
   public boolean isLiteral()
   {
      return m_bLiteral;
   }

   /**
    * Sets the denormalization destination flag.
    * @param bDenormalized The denormalization destination flag to set.
    */
   public void setDenormalized(boolean bDenormalized)
   {
      verifyNotReadOnly();
      m_bDenormalized = bDenormalized;
   }

   /**
    * @return The denormalization destination flag.
    */
   public boolean isDenormalized()
   {
      return m_bDenormalized;
   }

   /**
    * Sets the containing table.
    * @param table The containing table to set.
    */
   public void setTable(Table table)
   {
      verifyNotReadOnly();
      m_table = table;
   }

   /**
    * @return The containing table.
    */
   public Table getTable()
   {
      return m_table;
   }

   /**
    * Sets the column type.
    * @param type The column type to set.
    */
   public void setType(Primitive type)
   {
      super.setType(type);
      m_valueType = type;

      if (type != null && type != Primitive.STRING)
      {
         setCaseInsensitive(false);
      }
   }

   /**
    * Sets the column value type.
    * @param valueType The column value type to set.
    */
   public void setValueType(Primitive valueType)
   {
      verifyNotReadOnly();
      m_valueType = valueType;
   }

   /**
    * @return The column value type.
    */
   public Primitive getValueType()
   {
      return m_valueType;
   }

   /**
    * Sets the type conversion component.
    * @param converter The type conversion component to set.
    */
   public void setConverter(Component converter)
   {
      verifyNotReadOnly();
      m_converter = converter;

      if (converter != null)
      {
         if (m_type == null)
         {
            throw new MetadataException("err.meta.converterUse");
         }

         Object instance = converter.getInstance(null);

         if (!(instance instanceof Converter))
         {
            throw new MetadataException("err.meta.invalidConverter", new Object[]{converter.getName()});
         }

         m_valueType = ((Converter)instance).getDestinationType();
         m_bTimeZoned = (m_valueType == Primitive.TIMESTAMP && instance instanceof CalendarFactory);
         m_bPostConverted = !(instance instanceof SQLConverter);
      }
      else
      {
         m_valueType = m_type;
         m_bTimeZoned = false;
         m_bPostConverted = false;
      }
   }

   /**
    * @return The type conversion component.
    */
   public Component getConverter()
   {
      return m_converter;
   }

   /**
    * @return True if the time zone is converted.
    */
   public boolean isTimeZoned()
   {
      return m_bTimeZoned;
   }

   /**
    * @return True if the value cannot be converted directly by the database. 
    */
   public boolean isPostConverted()
   {
      return m_bPostConverted;
   }

   /**
    * Computes the column type.
    */
   public void computeType()
   {
      verifyNotReadOnly();
      
      if (m_type == null)
      {
         Attribute attribute = null;

         for (Iterator itr = m_table.getMappingArrayIterator(); itr.hasNext();)
         {
            RelationalPrimitiveMapping[] mappings = (RelationalPrimitiveMapping[])itr.next();
            RelationalPrimitiveMapping mapping = mappings[m_nOrdinal];

            if (mapping != null)
            {
               Primitive type = (Primitive)mapping.getAttribute().getType();

               if (type != Primitive.ANY)
               {
                  if (attribute != null && m_type != type)
                  {
                     throw new MetadataException("err.meta.ambiguousColumnTypeMapping",
                        new Object[]{getName(), getTable().getName(),
                           mapping.getAttribute().getName(),
                           mapping.getAttribute().getMetaclass().getName(),
                           attribute.getName(),
                           attribute.getMetaclass().getName()});
                  }
   
                  m_type = type;
                  attribute = mapping.getAttribute();
               }
            }
         }

         if (m_type == null)
         {
            throw new MetadataException("err.meta.unknownColumnType",
               new Object[]{getName(), getTable().getName()});
         }

         if (m_valueType == null)
         {
            m_valueType = m_type;
         }
      }
   }

   /**
    * Adds a mapped attribute to the column.
    * @param attribute The attribute to add.
    */
   public void addAttribute(Attribute attribute)
   {
      verifyNotReadOnly();
      
      Metaclass metaclass = attribute.getRootDeclarator();

      if (m_lastMetaclass != null)
      {
         if (!metaclass.isUpcast(m_lastMetaclass) && !m_lastMetaclass.isUpcast(metaclass))
         {
            m_bMultiplexed = true;
         }
      }
      else
      {
         m_lastMetaclass = metaclass;
      }
      
      setRequired(attribute.isRequired(), false);
   }

   /**
    * Sets a flag that the column is required.
    * @param bRequired The flag to set.
    * @param bOverride True to override the primary flag.
    */
   public void setRequired(boolean bRequired, boolean bOverride)
   {
      verifyNotReadOnly();

      if (!bOverride)
      {
         bRequired |= m_bPrimary;
      }

      if (bRequired)
      {
         if (!m_bRequiredInitialized)
         {
            m_bRequired = true;
            m_bRequiredInitialized = true;
         }
      }
      else
      {
         if (m_bRequiredInitialized)
         {
            m_bRequired = false;
         }
         else
         {
            m_bRequiredInitialized = true;
         }
      }
   }
   
   /**
    * @return True if the column is shared between two or
    * more metaclasses from different hierarchies.
    */
   public boolean isMultiplexed()
   {
      return m_bMultiplexed;
   }

   /**
    * @return True if the column is inherited.
    */
   public boolean isInherited()
   {
      for (int i = 0, n = m_table.getAspectCount(); i < n; ++i)
      {
         if (((Table)m_table.getAspect(i)).findColumn(m_sName) != null)
         {
            return true;
         }
      }

      return false;
   }

   /**
    * @see nexj.core.meta.persistence.sql.ColumnBase#copyTo(nexj.core.meta.persistence.sql.ColumnBase)
    */
   public void copyTo(Column target)
   {
      super.copyTo(target);
      target.setCaseInsensitive(isCaseInsensitive());
      target.setConverter(getConverter());
      target.setLiteral(isLiteral());
      target.setNullable(isNullable());
   }

   /**
    * @see java.lang.Comparable#compareTo(java.lang.Object)
    */
   public int compareTo(Object obj)
   {
      int n = m_table.compareTo(((Column)obj).m_table);

      if (n == 0)
      {
         n = super.compareTo(obj);
      }

      return n;
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      writer.write("Column ");

      if (m_table != null)
      {
         writer.write(m_table.getName());
      }

      writer.write('.');
      writer.write(m_sName);
   }

   /**
    * @see nexj.core.meta.NamedMetadataObject#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }
}
