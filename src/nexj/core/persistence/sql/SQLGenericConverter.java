// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.sql;

import nexj.core.meta.Primitive;
import nexj.core.meta.UnaryFunction;
import nexj.core.persistence.Converter;
import nexj.core.persistence.Field;

/**
 * Generic SQL Converter implementation.
 */
public class SQLGenericConverter implements Converter, SQLConverter
{
   // attributes
   
   /**
    * True if the sort order is preserved during the conversion.
    */
   private boolean m_bOrderPreserved;
   
   // associations
   
   /**
    * Source type.
    */
   private Primitive m_fromType;
   
   /**
    * Destination type.
    */
   private Primitive m_toType;
   
   /**
    * The forward conversion function.
    */
   private UnaryFunction m_forward;
   
   /**
    * The inverse conversion function.
    */
   private UnaryFunction m_inverse;

   // constructors

   /**
    * Constructs the converter.
    * @param fromType The source type.
    * @param toType The destination type.
    * @param forward The forward conversion function.
    * @param inverse The inverse conversion function.
    * @param bOrderPreserved True if the sort order is preserved during the conversion.
    */
   public SQLGenericConverter(Primitive fromType, Primitive toType,
      UnaryFunction forward, UnaryFunction inverse, boolean bOrderPreserved)
   {
      m_fromType = fromType;
      m_toType = toType;
      m_forward = forward;
      m_inverse = inverse;
      m_bOrderPreserved = bOrderPreserved;
   }

   // operations
   
   /**
    * @see nexj.core.persistence.Converter#getDestinationType()
    */
   public Primitive getDestinationType()
   {
      return m_toType;
   }

   /**
    * @see nexj.core.persistence.Converter#getForwardFunction()
    */
   public UnaryFunction getForwardFunction()
   {
      return m_forward;
   }

   /**
    * @see nexj.core.persistence.Converter#getSourceType()
    */
   public Primitive getSourceType()
   {
      return m_fromType;
   }

   /**
    * @see nexj.core.persistence.Converter#getInverseFunction()
    */
   public UnaryFunction getInverseFunction()
   {
      return m_inverse;
   }

   /**
    * @see nexj.core.persistence.Converter#isOrderPreserved()
    */
   public boolean isOrderPreserved()
   {
      return m_bOrderPreserved;
   }

   /**
    * @see nexj.core.persistence.sql.SQLConverter#appendConversion(java.lang.StringBuffer, nexj.core.persistence.Field, nexj.core.persistence.sql.SQLGenerator)
    */
   public void appendConversion(StringBuffer buf, Field field, SQLGenerator gen)
   {
      gen.getAdapter().appendTypeConversion(buf, field, m_fromType, m_toType, gen);
   }
}
