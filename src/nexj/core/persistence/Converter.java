// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.persistence;

import nexj.core.meta.Primitive;
import nexj.core.meta.UnaryFunction;

/**
 * Interface for custom type conversion.
 */
public interface Converter
{
   /**
    * @return The forward conversion result type.
    */
   Primitive getDestinationType();
   
   /**
    * @return The forward transformation function.
    */
   UnaryFunction getForwardFunction();
   
   /**
    * @return The forward conversion result type.
    */
   Primitive getSourceType();
   
   /**
    * @return The inverse transformation function.
    */
   UnaryFunction getInverseFunction();
   
   /**
    * @return True if the conversion preserves the sort order.
    */
   boolean isOrderPreserved();
}
