// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;


// inner classes

/**
 * Unary operator descriptor containing type conversion information.
 */
public class UnaryDescriptor
{
   // attribute

   /**
    * The operand type after conversion.
    */
   private Primitive m_operandType;

   /**
    * The result type.
    */
   private Primitive m_resultType;

   // constructors

   /**
    * Constructs the descriptor.
    * @param operandType The operand type.
    * @param resultType The result type.
    */
   public UnaryDescriptor(Primitive operandType, Primitive resultType)
   {
      m_operandType = operandType;
      m_resultType = resultType;
   }
   
   /**
    * Constructs the descriptor.
    * @param type Operand and result type.
    */
   public UnaryDescriptor(Primitive type)
   {
      m_operandType = m_resultType = type;
   }
   
   // operations

   /**
    * @return The operand type after conversion.
    */
   public Primitive getOperandType()
   {
      return m_operandType;
   }
   
   /**
    * @return The result type.
    */
   public Primitive getResultType()
   {
      return m_resultType;
   }
}