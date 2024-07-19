// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Primitive;
import nexj.core.meta.UnaryFunction;
import nexj.core.runtime.Initializable;
import nexj.core.util.ObjUtil;

/**
 * Generic converter for primitive types.
 */
public class PrimitiveConverter implements Converter, Initializable
{
   // attributes

   /**
    * True if the sort order is preserved after conversion.
    */
   private boolean m_bOrderPreserved;
   
   // associations
   
   /**
    * The source type.
    */
   private Primitive m_sourceType;

   /**
    * The destination type.
    */
   private Primitive m_destinationType;
   
   /**
    * The forward conversion function.
    */
   private UnaryFunction m_forwardFunction;

   /**
    * The inverse conversion function.
    */
   private UnaryFunction m_inverseFunction;

   // constructors

   /**
    * Constructs an uninitialized converter.
    */
   public PrimitiveConverter()
   {
   }

   /**
    * Constructs an initialized converter.
    * @param src The source type.
    * @param dst The destination type.
    */
   public PrimitiveConverter(Primitive src, Primitive dst)
   {
      m_sourceType = src;
      m_destinationType = dst;

      try
      {
         initialize();
      }
      catch (Exception e)
      {
         ObjUtil.rethrow(e);
      }
   }

   // operations

   /**
    * @see nexj.core.persistence.Converter#getDestinationType()
    */
   public Primitive getDestinationType()
   {
      return m_destinationType;
   }

   /**
    * @see nexj.core.persistence.Converter#getForwardFunction()
    */
   public UnaryFunction getForwardFunction()
   {
      return m_forwardFunction;
   }

   /**
    * @see nexj.core.persistence.Converter#getSourceType()
    */
   public Primitive getSourceType()
   {
      return m_sourceType;
   }

   /**
    * @see nexj.core.persistence.Converter#getInverseFunction()
    */
   public UnaryFunction getInverseFunction()
   {
      return m_inverseFunction;
   }

   /**
    * @see nexj.core.persistence.Converter#isOrderPreserved()
    */
   public boolean isOrderPreserved()
   {
      return m_bOrderPreserved;
   }
   
   /**
    * Sets the source type.
    * @param sourceType The source type to set.
    */
   public void setSourceType(Primitive sourceType)
   {
      m_sourceType = sourceType;
   }

   /**
    * Sets the destination type.
    * @param destinationType The destination type to set.
    */
   public void setDestinationType(Primitive destinationType)
   {
      m_destinationType = destinationType;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      m_forwardFunction = Primitive.getConverter(m_sourceType, m_destinationType);
      m_inverseFunction = Primitive.getConverter(m_destinationType, m_sourceType);
      m_bOrderPreserved = Primitive.isOrderPreserved(m_sourceType, m_destinationType);
   }
}
