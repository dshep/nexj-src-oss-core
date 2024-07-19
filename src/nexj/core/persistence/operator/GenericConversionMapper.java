// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;
import nexj.core.persistence.Operator;

/**
 * Generic implementation of the descriptor map.
 */
public class GenericConversionMapper implements ConversionMapper, Cloneable
{
   // associations

   /**
    * 2d array of unary descriptors: UnaryDescriptor[nOperatorOrdinal * Primitive.MAX_COUNT + nTypeOrdinal].
    */
   private UnaryDescriptor[] m_unaryDescriptorArray = new UnaryDescriptor[Operator.MAX_COUNT * Primitive.MAX_COUNT];

   /**
    * 3d array of binary descriptors: BinaryDescriptor[
    * (nOperatorOrdinal * Primitive.MAX_COUNT + nLeftOrdinal) * Primitive.MAX_COUNT + nRightOrdinal].
    */
   private BinaryDescriptor[] m_binaryDescriptorArray = new BinaryDescriptor[Operator.MAX_COUNT * Primitive.MAX_COUNT * Primitive.MAX_COUNT];

   // constructors
   
   /**
    * Constructs an empty conversion mapper.
    */
   public GenericConversionMapper()
   {
   }
   
   /**
    * Constructs a conversion mapper initialized from another mapper.
    * @param mapper The source mapper.
    */
   public GenericConversionMapper(GenericConversionMapper mapper)
   {
      System.arraycopy(mapper.m_unaryDescriptorArray, 0, m_unaryDescriptorArray, 0, m_unaryDescriptorArray.length);
      System.arraycopy(mapper.m_binaryDescriptorArray, 0, m_binaryDescriptorArray, 0, m_binaryDescriptorArray.length);
   }
   
   // operations
   
   /**
    * @see nexj.core.persistence.operator.ConversionMapper#getType(nexj.core.meta.Primitive)
    */
   public Primitive getType(Primitive type)
   {
      return type;
   }

   /**
    * @see nexj.core.persistence.ConversionMapper#getUnaryDescriptor(nexj.core.persistence.operator.UnaryOperator)
    */
   public UnaryDescriptor getUnaryDescriptor(UnaryOperator op)
   {
      return m_unaryDescriptorArray[getUnaryIndex(op.getOrdinal(), (Primitive)op.getOperand().getType())];
   }

   /**
    * Calculates the unary descriptor index in the array.
    * @param nOrdinal The unary operator ordinal number.
    * @param type The operand type.
    * @return The calculated index. 
    */
   private final static int getUnaryIndex(int nOrdinal, Primitive type)
   {
      return nOrdinal * Primitive.MAX_COUNT + type.getOrdinal();
   } 

   /**
    * @see nexj.core.persistence.ConversionMapper#getBinaryDescriptor(nexj.core.persistence.operator.BinaryOperator)
    */
   public BinaryDescriptor getBinaryDescriptor(BinaryOperator op)
   {
      return m_binaryDescriptorArray[getBinaryIndex(op.getOrdinal(), (Primitive)op.getLeft().getType(), (Primitive)op.getRight().getType())];
   }

   /**
    * Calculates the binary descriptor index in the array.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand type.
    * @param rightType The right operand type.
    * @return The calculated index. 
    */
   private final static int getBinaryIndex(int nOrdinal, Primitive leftType, Primitive rightType)
   {
      return ((nOrdinal * Primitive.MAX_COUNT) + leftType.getOrdinal()) * Primitive.MAX_COUNT + rightType.getOrdinal();
   }

   /**
    * @see nexj.core.persistence.operator.ConversionMapper#getIfDescriptor(nexj.core.persistence.operator.IfOperator)
    */
   public BinaryDescriptor getIfDescriptor(IfOperator op)
   {
      return m_binaryDescriptorArray[getBinaryIndex(op.getOrdinal(), (Primitive)op.getThen().getType(), (Primitive)op.getElse().getType())];
   }

   /**
    * Sets the descriptor for a given unary operator.
    * @param nOrdinal The unary operator ordinal number.
    * @param type The operand type.
    * @param descr The unary descriptor.
    */
   public void setUnaryDescriptor(int nOrdinal, Primitive type, UnaryDescriptor descr)
   {
      m_unaryDescriptorArray[getUnaryIndex(nOrdinal, type)] = descr;
   }

   /**
    * Creates and sets the descriptor for a given unary operator.
    * @param nOrdinal The unary operator ordinal number.
    * @param opType The operand type.
    * @param resType The result type.
    */
   public void setUnaryDescriptor(int nOrdinal, Primitive opType, Primitive resType)
   {
      setUnaryDescriptor(nOrdinal, opType, new UnaryDescriptor(opType, resType));
   }
   
   /**
    * Creates and sets the descriptor for a given unary operator.
    * @param nOrdinal The unary operator ordinal number.
    * @param type The operand and result type.
    */
   public void setUnaryDescriptor(int nOrdinal, Primitive type)
   {
      setUnaryDescriptor(nOrdinal, type, new UnaryDescriptor(type));
   }
   
   /**
    * Creates and sets a range of descriptors for a given unary operator.
    * @param nOrdinal The unary operator ordinal number.
    * @param typeArray Array of operand types, which are also result types.
    */
   public void setUnaryDescriptor(int nOrdinal, Primitive[] typeArray)
   {
      for (int i = 0; i < typeArray.length; ++i)
      {
         setUnaryDescriptor(nOrdinal, typeArray[i]);
      }
   }
   
   /**
    * Sets the descriptor for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand type.
    * @param rightType The right operand type.
    * @param descr The descriptor.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive leftType, Primitive rightType, BinaryDescriptor descr)
   {
      m_binaryDescriptorArray[getBinaryIndex(nOrdinal, leftType, rightType)] = descr;
   }
   
   /**
    * Creates and sets the descriptor for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand type.
    * @param rightType The right operand type.
    * @param leftCvtType The left converted type.
    * @param rightCvtType The right converted type.
    * @param resType The result type.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive leftType, Primitive rightType,
      Primitive leftCvtType, Primitive rightCvtType, Primitive resType, boolean bSymmetric)
   {
      setBinaryDescriptor(nOrdinal, leftType, rightType, new BinaryDescriptor(leftCvtType, rightCvtType, resType));
      
      if (bSymmetric && leftType != rightType)
      {
         setBinaryDescriptor(nOrdinal, rightType, leftType, new BinaryDescriptor(rightCvtType, leftCvtType, resType));
      }
   }
   
   /**
    * Creates and sets the descriptor for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand, converted and result type.
    * @param rightType The right operand type.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive leftType, Primitive rightType, boolean bSymmetric)
   {
      setBinaryDescriptor(nOrdinal, leftType, rightType, leftType, leftType, leftType, bSymmetric);
   }
   
   /**
    * Creates and sets a range of descriptors for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand, converted and result type.
    * @param rightTypeArray The right operand type array.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive leftType, Primitive[] rightTypeArray, boolean bSymmetric)
   {
      for (int i = 0; i < rightTypeArray.length; ++i)
      {
         setBinaryDescriptor(nOrdinal, leftType, rightTypeArray[i], bSymmetric);
      }
   }

   /**
    * Creates and sets a range of descriptors for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param leftType The left operand and converted type.
    * @param rightTypeArray The right operand type array.
    * @param resType The result type.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive leftType,
      Primitive[] rightTypeArray, Primitive resType, boolean bSymmetric)
   {
      for (int i = 0; i < rightTypeArray.length; ++i)
      {
         setBinaryDescriptor(nOrdinal, leftType, rightTypeArray[i], leftType, leftType, resType, bSymmetric);
      }
   }
   
   /**
    * Creates and sets a matrix of descriptors for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param typeArray The operand type array. The right operand type is obtained by
    * enumerating all the operand types with index not exceeding the left one.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive[] typeArray, boolean bSymmetric)
   {
      for (int i = 0; i < typeArray.length; ++i)
      {
         for (int k = 0; k <= i; ++k)
         {
            setBinaryDescriptor(nOrdinal, typeArray[i], typeArray[k], bSymmetric);
         }
      }
   }
   
   /**
    * Creates and sets a matrix of descriptors for a given binary operator.
    * @param nOrdinal The binary operator ordinal number.
    * @param typeArray The operand type array. The right operand type is obtained by
    * enumerating all the operand types with index not exceeding the left one.
    * @param resType The result type.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int nOrdinal, Primitive[] typeArray, Primitive resType, boolean bSymmetric)
   {
      for (int i = 0; i < typeArray.length; ++i)
      {
         for (int k = 0; k <= i; ++k)
         {
            setBinaryDescriptor(nOrdinal, typeArray[i], typeArray[k], typeArray[i], typeArray[i], resType,  bSymmetric);
         }
      }
   }

   /**
    * Creates and sets a matrix of descriptors for a given binary operators.
    * @param ordinalArray The binary operator ordinal number array.
    * @param typeArray The operand type array. The right operand type is obtained by
    * enumerating all the operand types with index not exceeding the left one.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int[] ordinalArray, Primitive[] typeArray, boolean bSymmetric)
   {
      for (int n = 0; n < ordinalArray.length; ++n)
      {
         for (int i = 0; i < typeArray.length; ++i)
         {
            for (int k = 0; k <= i; ++k)
            {
               setBinaryDescriptor(ordinalArray[n], typeArray[i], typeArray[k], bSymmetric);
            }
         }
      }
   }
   
   /**
    * Creates and sets a matrix of descriptors for a given binary operators.
    * @param ordinalArray The binary operator ordinal number array.
    * @param typeArray The operand type array. The right operand type is obtained by
    * enumerating all the operand types with index not exceeding the left one.
    * @param resType The result type.
    * @param bSymmetric True if this should be applied also with swapped left and right types.
    */
   public void setBinaryDescriptor(int[] ordinalArray, Primitive[] typeArray, Primitive resType, boolean bSymmetric)
   {
      for (int n = 0; n < ordinalArray.length; ++n)
      {
         for (int i = 0; i < typeArray.length; ++i)
         {
            for (int k = 0; k <= i; ++k)
            {
               setBinaryDescriptor(ordinalArray[n], typeArray[i], typeArray[k], typeArray[i], typeArray[i], resType,  bSymmetric);
            }
         }
      }
   }

   /**
    * @see java.lang.Object#clone()
    */
   protected Object clone()
   {
      return new GenericConversionMapper(this);
   }
   
   
}
