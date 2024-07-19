package nexj.core.scripting;

import nexj.test.junit.EvalTestCase;

/**
 * Tests the dynamic object system.
 */
public class ObjectTest extends EvalTestCase
{
   // tests

   /**
    * Tests for an attribute resolution bug. The overrides in class 1B weren't working, so
    * (OverrideTest1B'x) and (OverrideTest1B'y) would return null.
    */
   public void testStaticAttributeOverride() throws Exception
   {
      assertEquals("hi", eval("(OverrideTest2B'x)"));
      assertEquals("bye", eval("(OverrideTest2B'y)"));
      assertEquals("hi", eval("(OverrideTest1B'x)"));
      assertEquals("bye", eval("(OverrideTest1B'y)"));
   }
}
