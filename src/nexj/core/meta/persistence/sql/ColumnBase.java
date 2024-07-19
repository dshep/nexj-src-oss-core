// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta.persistence.sql;

import nexj.core.meta.DocumentedNamedMetadataObject;
import nexj.core.meta.MetadataException;
import nexj.core.meta.Primitive;

/**
 * Base class with type information for column and column outline.
 */
public abstract class ColumnBase extends DocumentedNamedMetadataObject
{
   // constants

   /**
    * Fixed storage allocation.
    */
   public final static byte FIXED = 0;
   
   /**
    * Varying storage allocation.
    */
   public final static byte VARYING = 1;

   /**
    * Locator storage allocation (BLOB, CLOB).
    */
   public final static byte LOCATOR = 2;

   // attributes

   /**
    * The decimal precision of a numeric column or the length of a text column.
    */
   protected int m_nPrecision;

   /**
    * The decimal scale of a numeric column.
    */
   protected int m_nScale;

   /**
    * The allocation type - FIXED, VARYING or LOB.
    */
   protected byte m_nAllocation = FIXED;

   // associations

   /**
    * The type of the column. Each primitive type is mapped to an underlying
    * database type.
    */
   protected Primitive m_type;

   // constructors

   /**
    * Constructs the base.
    * @param sName The column name.
    */
   public ColumnBase(String sName)
   {
      super(sName);
   }

   /**
    * Constructs the base.
    */
   public ColumnBase()
   {
   }

   // operations

   /**
    * Sets the column type.
    * @param type The column type to set.
    */
   public void setType(Primitive type)
   {
      verifyNotReadOnly();
      m_type = type;
   }

   /**
    * @return The column type.
    */
   public Primitive getType()
   {
      return m_type;
   }
   
   /**
    * Sets the decimal precision or text length.
    * @param nPrecision The decimal precision or text length to set.
    */
   public void setPrecision(int nPrecision)
   {
      verifyNotReadOnly();
      m_nPrecision = nPrecision;
   }

   /**
    * @return The decimal precision or text length.
    */
   public int getPrecision()
   {
      return m_nPrecision;
   }

   /**
    * Computes the column precision based on the maximum supported precision.
    * @param nMaxPrecision The maximum supported precision.
    * @return The decimal precision or text length.
    */
   public int getPrecision(int nMaxPrecision)
   {
      if (m_nPrecision == 0 || m_nPrecision > nMaxPrecision)
      {
         return nMaxPrecision;
      }

      return m_nPrecision;
   }

   /**
    * Sets the decimal scale of a numeric column.
    * @param nScale The decimal scale to set.
    */
   public void setScale(int nScale)
   {
      verifyNotReadOnly();
      m_nScale = nScale;
   }

   /**
    * @return The decimal scale of a numeric column.
    */
   public int getScale()
   {
      return m_nScale;
   }

   /**
    * Computes the column scale based on the maximum supported precision.
    * @param nMaxPrecision The maximum supported precision.
    * @return The decimal scale of a numeric column.
    */
   public int getScale(int nMaxPrecision)
   {
      return Math.min(nMaxPrecision, m_nScale);
   }

   /**
    * Sets the allocation type - FIXED, VARYING or LOB.
    * @param nAllocation The allocation type - FIXED, VARYING or LOCATOR to set.
    */
   public void setAllocation(byte nAllocation)
   {
      verifyNotReadOnly();
      m_nAllocation = nAllocation;
   }

   /**
    * @return The allocation type - FIXED, VARYING or LOB.
    */
   public byte getAllocation()
   {
      return m_nAllocation;
   }

   /**
    * @return The allocation string.
    */
   public String getAllocationString()
   {
      switch (m_nAllocation)
      {
         case FIXED:
            return "fixed";

         case VARYING:
            return "varying";

         case LOCATOR:
            return "locator";

         default:
            return null;
      }
   }

   /**
    * Sets the column type attributes.
    * @param type The column type.
    * @param precision The precision. Can be null.
    * @param scale The scale. Can be null.
    * @param nAllocation The column allocation, one of the Column.* constants or -1 for unspecified.
    * @throws MetadataException if the attribute combination is invalid. 
    */
   public void setType(Primitive type, Integer precision,
      Integer scale, byte nAllocation) throws MetadataException
   {
      setType(type);

      if (Primitive.STRING.equals(type) ||
         Primitive.BINARY.equals(type))
      {
         if (precision != null)
         {
            setPrecision(precision.intValue());
            
            if (m_nPrecision <= 0)
            {
               throw new MetadataException("err.meta.precision", new Object[]{precision});
            }
         }

         if (scale != null)
         {
            throw new MetadataException("err.meta.scaleUse");
         }

         if (nAllocation < 0)
         {
            nAllocation = Column.VARYING;
         }

         setAllocation(nAllocation);
      }
      else
      {
         if (Primitive.DECIMAL.equals(type))
         {
            if (precision != null)
            {
               setPrecision(precision.intValue());
            
               if (m_nPrecision <= 0 || m_nPrecision > 38)
               {
                  throw new MetadataException("err.meta.precision", new Object[]{precision});
               }
            }

            if (scale != null)
            {
               setScale(scale.intValue());
               
               if (m_nScale < 0 ||
                  m_nScale > m_nPrecision &&
                  m_nPrecision != 0)
               {
                  throw new MetadataException("err.meta.scale", new Object[]{scale});
               }
            }
         }
         else
         {
            if (precision != null)
            {
               if (Primitive.INTEGER.equals(type))
               {
                  setPrecision(precision.intValue());

                  if (m_nPrecision == 4)
                  {
                     m_nPrecision = 0;
                  }
                  else if (m_nPrecision != 1 && m_nPrecision != 2)
                  {
                     throw new MetadataException("err.meta.integerPrecision", new Object[]{precision});
                  }
               }
               else
               {
                  throw new MetadataException("err.meta.precisionUse");
               }
            }

            if (scale != null)
            {
               throw new MetadataException("err.meta.scaleUse");
            }
         }

         if (nAllocation >= 0)
         {
            throw new MetadataException("err.meta.allocationUse");
         }
      }
      
      if (precision != null)
      {
         setPrecision(precision.intValue());
      }

      if (scale != null)
      {
         setScale(scale.intValue());
      }

      if (nAllocation >= 0)
      {
         setAllocation(nAllocation);
      }
   }

   /**
    * Copies the column attributes to a column.
    * @param target The destination column.
    */
   public void copyTo(ColumnBase target)
   {
      target.setAllocation(getAllocation());
      target.setDescription(getDescription());
      target.setName(getName());
      target.setPrecision(getPrecision());
      target.setScale(getScale());
      target.setType(getType());
   }

   /**
    * Determine if this column is a "Large Object" column based on Adapter maximum supported length.
    * @param nMaxStringLength The maximum non-LOB string length supported by Adapter.
    * @param nMaxBinaryLength The maximum non-LOB binary length supported by Adapter.
    * @return The column requires LOB storage.
    */
   public boolean isLOB(int nMaxStringLength, int nMaxBinaryLength)
   {
      if (m_type == null)
      {
         return false; // can occur during validation if type not set
      }

      switch (m_type.getOrdinal())
      {
         case Primitive.STRING_ORDINAL:
            return m_nPrecision <= 0 ||
                   m_nPrecision > nMaxStringLength ||
                   m_nAllocation == Column.LOCATOR;

         case Primitive.BINARY_ORDINAL:
            return m_nPrecision <= 0 ||
                   m_nPrecision > nMaxBinaryLength ||
                   m_nAllocation == Column.LOCATOR;

         default:
            return false;
      }
   }
}
