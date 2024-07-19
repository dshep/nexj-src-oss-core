// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

/**
 * Interface implemented by functions of two arguments.
 */
public interface BinaryFunction
{
   /**
    * Invokes the function.
    * @param left The left argument.
    * @param right The right argument.
    * @return The function result.
    */
   Object invoke(Object left, Object right);
}
