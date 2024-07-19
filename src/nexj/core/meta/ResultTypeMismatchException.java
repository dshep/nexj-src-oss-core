package nexj.core.meta;

import nexj.core.util.Named;

/**
 * Exception thrown when there is a mismatch in the type of the value returned by a method.
 * Used in the typed extension to the dynamic object system.
 */
public class ResultTypeMismatchException extends TypeMismatchException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 8092943004825391913L;

   // constructors

   /**
    * Constructs a method return type mismatch exception.
    * @param method The method that is returning.
    * @param location The instance (or other) holding the method.
    * @param expectedType The expected type.
    * @param actualType The actual type.
    */
   public ResultTypeMismatchException(Named method, Object location, Object expectedType, Object actualType)
   {
      super("err.meta.returnTypeMismatch", new Object[]{method, location, expectedType, actualType});
   }
}
