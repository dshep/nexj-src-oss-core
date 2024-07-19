// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;


// inner classes

/**
 * Binary operator descriptor containing type conversion information.
 */
public class BinaryDescriptor
{
   // associations
   
   /**
    * The left operand type.
    */
   private Primitive m_leftType;

   /**
    * The right operand type.
    */
   private Primitive m_rightType;

   /**
    * The result type.
    */
   private Primitive m_resultType;

   // constructors
   
   /**
    * Constructs the descriptor.
    * @param leftType The left operand type.
    * @param rightType The right operand type.
    * @param resultType The result type.
    */
   public BinaryDescriptor(Primitive leftType, Primitive rightType, Primitive resultType)
   {
      m_leftType = leftType;
      m_rightType = rightType;
      m_resultType = resultType;
   }
   
   /**
    * Constructs the descriptor.
    * @param operandType The left and right operand type.
    * @param resultType The result type.
    */
   public BinaryDescriptor(Primitive operandType, Primitive resultType)
   {
      m_leftType = m_rightType = operandType;
      m_resultType = resultType;
   }
   
   /**
    * Constructs the descriptor.
    * @param type The operand and result type.
    */
   public BinaryDescriptor(Primitive type)
   {
      m_leftType = m_rightType = m_resultType = type;
   }

   // operations

   /**
    * @return The left operand type.
    */
   public Primitive getLeftType()
   {
      return m_leftType;
   }
   
   /**
    * @return The right operand type.
    */
   public Primitive getRightType()
   {
      return m_rightType;
   }
   
   /**
    * Sets the result type.
    * @param resultType The result type to set.
    */
   public void setResultType(Primitive resultType)
   {
      m_resultType = resultType;
   }

   /**
    * @return The result type.
    */
   public Primitive getResultType()
   {
      return m_resultType;
   }
}