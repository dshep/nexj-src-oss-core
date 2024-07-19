// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence.operator;

import nexj.core.meta.Primitive;

/**
 * Interface for handling operator type conversions.
 */
public interface ConversionMapper
{
   /**
    * Gets the persistence storage type corresponding to a given system type.
    * @param type The system type.
    * @return The corresponding persistence storage type.
    */
   Primitive getType(Primitive type);

   /**
    * Gets the descriptor of the specified unary operator.
    * @param op The unary operator.
    * @return The unary operator descriptor, or null if the operator is not supported.
    */
   UnaryDescriptor getUnaryDescriptor(UnaryOperator op);

   /**
    * Gets the descriptor of the specified binary operator.
    * @param op The binary operator.
    * @return The binary operator descriptor, or null if the operator is not supported.
    */
   BinaryDescriptor getBinaryDescriptor(BinaryOperator op);

   /**
    * Gets the descriptor of an if operator.
    * @param op The if operator.
    * @return The binary operator descriptor, or null if the operator is not supported.
    */
   BinaryDescriptor getIfDescriptor(IfOperator op);
}