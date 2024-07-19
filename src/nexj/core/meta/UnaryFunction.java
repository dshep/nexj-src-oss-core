// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

/**
 * Interface implemented by functions with one argument.
 */
public interface UnaryFunction
{
   /**
    * Invokes the function.
    * @param arg The function argument.
    * @return The function result.
    */
   Object invoke(Object arg);
}
