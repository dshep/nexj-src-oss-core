// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Primitive;
import nexj.core.meta.UnaryFunction;
import nexj.core.runtime.Initializable;

/**
 * Converts a special non-null value to null.
 */
public class NullConverter implements Converter, Initializable
{
   // attributes

   /**
    * The value substituted for null.
    */
   protected Object m_value;

   // associations
   
   /**
    * The primitive type.
    */
   protected Primitive m_type;

   /**
    * Converter to null.
    */
   protected UnaryFunction m_forwardFunction = new UnaryFunction()
   {
      public Object invoke(Object value)
      {
         if (m_value.equals(value))
         {
            return null;
         }

         return value;
      }
   };

   /**
    * Converter from null.
    */
   protected UnaryFunction m_inverseFunction = new UnaryFunction()
   {
      public Object invoke(Object value)
      {
         if (value == null)
         {
            return m_value;
         }

         return value;
      }
   };

   // operations
   
   /**
    * Sets the primitive type.
    * @param type The primitive type to set.
    */
   public void setType(Primitive type)
   {
      m_type = type;
   }

   /**
    * @return The primitive type.
    */
   public Primitive getType()
   {
      return m_type;
   }

   /**
    * Sets the value substituted for null.
    * @param sValue The value substituted for null to set.
    */
   public void setValue(String sValue)
   {
      m_value = sValue;
   }

   /**
    * @return The value substituted for null.
    */
   public Object getValue()
   {
      return m_value;
   }

   /**
    * @see nexj.core.persistence.Converter#getSourceType()
    */
   public Primitive getSourceType()
   {
      return m_type;
   }

   /**
    * @see nexj.core.persistence.Converter#getDestinationType()
    */
   public Primitive getDestinationType()
   {
      return m_type;
   }

   /**
    * @see nexj.core.persistence.Converter#getForwardFunction()
    */
   public UnaryFunction getForwardFunction()
   {
      return m_forwardFunction;
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
      return true;
   }

   /**
    * @see nexj.core.runtime.Initializable#initialize()
    */
   public void initialize() throws Exception
   {
      if (m_type == null)
      {
         throw new IllegalArgumentException("Type not specified");
      }

      if (m_value == null)
      {
         if (m_type == Primitive.STRING)
         {
            m_value = "";
         }
         else
         {
            throw new IllegalArgumentException("Value not specified");
         }
      }

      m_value = m_type.convert(m_value);
   }
}
