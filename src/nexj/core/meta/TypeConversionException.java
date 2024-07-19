// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import nexj.core.util.UncheckedException;

/**
 * Exception thrown when a type conversion fails.
 */
public class TypeConversionException extends UncheckedException
{
   // constants

   /**
    * Serialization version.  
    */
   private final static long serialVersionUID = -3448464370864566794L;

   // constructors

   /**
    * Creates the exception.
    * @param type The type, to which the conversion has failed.
    */
   public TypeConversionException(Type type)
   {
      super("err.meta.typeconv", new Object[]{type.getName()});
   }

   /**
    * Creates the exception.
    * @param type The type, to which the conversion has failed.
    * @param e The exception that has caused this exception.
    */
   public TypeConversionException(Type type, Throwable e)
   {
      super("err.meta.typeconv", new Object[]{type.getName()}, e);
   }
}
