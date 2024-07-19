package nexj.core.meta;

import nexj.core.util.Named;

/**
 * Exception thrown when there is a mismatch in the type of an argument passed to a method.
 * Used in the typed extension to the dynamic object system.
 */
public class ArgumentTypeMismatchException extends TypeMismatchException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 8092943004825391913L;

   // constructors

   /**
    * Constructs a method argument type mismatch exception.
    * @param method The method being called.
    * @param location The instance (or other) holding the method.
    * @param expectedType The expected type.
    * @param actualType The actual type.
    */
   public ArgumentTypeMismatchException(Named method, Object location, Object expectedType, Object actualType)
   {
      super("err.meta.argTypeMismatch", new Object[]{method, location, expectedType, actualType});
   }
}
