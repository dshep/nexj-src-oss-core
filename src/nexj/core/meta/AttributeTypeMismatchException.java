package nexj.core.meta;

import nexj.core.util.Named;

/**
 * Exception thrown when there is a mismatch in the type of a value assigned to an attribute.
 * Used in the typed extension to the dynamic object system.
 */
public class AttributeTypeMismatchException extends TypeMismatchException
{
   // constants

   /**
    * Serialization version.
    */
   private static final long serialVersionUID = 8092943004825391913L;

   // constructors

   /**
    * Constructs an attribute type mismatch exception.
    * @param attribute The attribute being assigned.
    * @param location The instance (or other) holding the attribute.
    * @param expectedType The expected type.
    * @param actualType The actual type.
    */
   public AttributeTypeMismatchException(Named attribute, Object location, Object expectedType, Object actualType)
   {
      super("err.meta.attrTypeMismatch", new Object[]{attribute, location, expectedType, actualType});
   }
}
