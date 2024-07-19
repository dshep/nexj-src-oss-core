package nexj.core.scripting;

import nexj.core.meta.ArgumentTypeMismatchException;
import nexj.core.meta.AttributeTypeMismatchException;
import nexj.core.meta.Primitive;
import nexj.core.meta.ResultTypeMismatchException;
import nexj.core.scripting.object.InvocationException;
import nexj.test.junit.EvalTestCase;

/**
 * Tests the type facility for the dynamic object system.
 */
public class TypedObjectTest extends EvalTestCase
{
   // operations

   public String getTypedClassName()
   {
      return "TypedTest1";
   }

   public String getInheriterClassName()
   {
      return "InheritedTypesTest";
   }

   // tests

   /**
    * Verifies that type checking works for method arguments & return values.
    */
   public void testTypedInstMethod() throws Exception
   {
      eval("(define inst (:: " + getTypedClassName() + "))");

      // assign double to int
      try
      {
         eval("(inst'instMeth1 5.4 5 inst)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // assign double to object
      try
      {
         eval("(inst'instMeth1 5 5 5.4)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // assign object to int
      try
      {
         eval("(inst'instMeth1 inst 5 inst)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      eval("(inst'instMeth1 5 42 inst)");
      eval("(inst'instMeth1 5 inst ())");
      eval("(inst'instMeth1 5 3.14 inst)");

      // Call with too few args
      try
      {
         eval("(inst'instMeth1 5)");
         fail("Expected InvocationException");
      }
      catch (InvocationException ex)
      {
      }

      // Call with too many args
      try
      {
         eval("(inst'instMeth1 5 52 inst 5)");
         fail("Expected InvocationException");
      }
      catch (InvocationException ex)
      {
      }

      /*
       * Testing collections.
       */

      eval("(inst'instMeth2 (collection 1 2 3))");

      // assign int to collection
      try
      {
         eval("(inst'instMeth2 1)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // assign collection with double to collection of int
      try
      {
         eval("(inst'instMeth2 (collection 1 2 3.14))");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // assign collection to an int
      try
      {
         eval("(inst'instMeth1 (collection 1 2 3) () ())");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }


      /*
       * Testing return value types.
       */
      eval("(inst'instMethGoodReturn)");

      try
      {
         eval("(inst'instMethBadReturn)");
         fail("Expected ReturnTypeMismatchException");
      }
      catch (ResultTypeMismatchException ex)
      {
      }
   }

   /**
    * Verifies that type checking works for method arguments & return values for class methods.
    * Doesn't need to run all the checks because that logic is already covered by the instance
    * method test.
    */
   public void testTypedClassMethod() throws Exception
   {
      eval("(" + getTypedClassName() + "'clsMeth1 42)");

      try
      {
         eval("(" + getTypedClassName() + "'clsMeth1 3.14)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }
   }

   /**
    * Verifies that type checking works for instance attributes.
    */
   public void testTypedInstAttr() throws Exception
   {
      eval("(define inst (:: " + getTypedClassName() + "))");
      assertNull(eval("(inst'instInt)"));
      eval("(inst'instInt 5)");

      // assign double to int
      try
      {
         eval("(inst'instInt 3.14)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      // assign double to object
      try
      {
         eval("(inst'instObj 3.14)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      eval("(define inst2 (:: " + getTypedClassName() + " :instInt 42))");

      // assign object to int
      try
      {
         eval("(inst'instInt inst2)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      eval("(inst'instObj inst2)");
      eval("(inst'instObj ())");
      eval("(inst'instAny inst2)");
      eval("(inst'instAny 3.14)");
      eval("(inst'instAny ())");


      /*
       * Testing collections.
       */

      eval("(inst'instIntArray (collection 1 2 3))");

      // assign int to collection
      try
      {
         eval("(inst'instIntArray 1)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      // assign collection with double to collection of int
      try
      {
         eval("(inst'instIntArray (collection 1 2 3.14))");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      // assign collection to an int
      try
      {
         eval("(inst'instInt (collection 1 2 3))");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }
   }

   /**
    * Verifies that type checking works for class attributes. Doesn't need to run the full suite of checks
    * as they are already covered by the instance attribute test.
    */
   public void testTypedClassAttr() throws Exception
   {
      eval("(" + getTypedClassName() + "'clsInt 42)");

      try
      {
         eval("(" + getTypedClassName() + "'clsInt 3.14)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }
   }

   /**
    * Verifies that members can be re-defined in derived classes, and that the types are pulled
    * from the base members.
    */
   public void testInheritedTypes() throws Exception
   {
      // Static attribute
      assertEquals(Primitive.createInteger(1337), eval("(" + getInheriterClassName() + "'clsInt)"));
      eval("(" + getInheriterClassName() + "'clsInt 42)");

      try
      {
         eval("(" + getInheriterClassName() + "'clsInt 3.14)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      // Static method
      assertEquals("override", eval("(" + getInheriterClassName() + "'clsMeth1 42)"));

      try
      {
         eval("(" + getInheriterClassName() + "'clsMeth1 \"abc\")");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // Instance attribute
      eval("(define inst (:: " + getInheriterClassName() + "))");
      assertEquals(Primitive.createInteger(1337), eval("(inst'instInt)"));
      eval("(inst'instInt 42)");

      try
      {
         eval("(inst'instInt 3.14)");
         fail("Expected AttributeTypeMismatchException");
      }
      catch (AttributeTypeMismatchException ex)
      {
      }

      // Instance method
      assertEquals("override", eval("(inst'instMeth1 42 3.14 inst)"));

      try
      {
         eval("(inst'instMeth1 42 3.14 3.14)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }
   }

   /**
    * Verifies that type checking works for variable arguments.
    */
   public void testVarArgMethod() throws Exception
   {
      eval("(define inst (:: " + getTypedClassName() + "))");

      // Call with no var arg
      eval("(inst'instMethVarArg 42)");

      // Call with one var arg
      eval("(inst'instMethVarArg 42 \"hi\")");

      // Call with three var args
      eval("(inst'instMethVarArg 42 \"hi\" \"bye\" \"why\")");

      // Call with bad var arg
      try
      {
         eval("(inst'instMethVarArg 42 51)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // Call with one bad, two good var args
      try
      {
         eval("(inst'instMethVarArg 42 \"hi\" 51 \"bye\")");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      // Call with too few args
      try
      {
         eval("(inst'instMethVarArg)");
         fail("Expected InvocationException");
      }
      catch (InvocationException ex)
      {
      }

      // Verify a minimum zero args var arg method
      eval("(inst'instMethVarArg2)");
      eval("(inst'instMethVarArg2 \"x\")");
      eval("(inst'instMethVarArg2 \"x\" \"xy\")");

      try
      {
         eval("(inst'instMethVarArg2 42)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }

      try
      {
         eval("(inst'instMethVarArg2 \"x\" 42)");
         fail("Expected ArgumentTypeMismatchException");
      }
      catch (ArgumentTypeMismatchException ex)
      {
      }
   }
}
