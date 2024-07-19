package nexj.core.scripting;

/**
 * Tests the typed exception object facility for the dynamic object system.
 */
public class TypedExceptionObjectTest extends TypedObjectTest
{
   // operations

   public String getTypedClassName()
   {
      return "TypedExceptionTest1";
   }

   public String getInheriterClassName()
   {
      return "InheritedExceptionTypesTest";
   }
}
