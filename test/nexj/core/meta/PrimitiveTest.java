// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import junit.framework.TestCase;

public class PrimitiveTest extends TestCase
{

   protected void setUp() throws Exception
   {
      super.setUp();
   }

   public void testCreateInteger()
   {
      assertEquals(-2, Primitive.createInteger(-2).intValue());
      assertEquals(-1, Primitive.createInteger(-1).intValue());
      assertEquals(0, Primitive.createInteger(0).intValue());
      assertEquals(1, Primitive.createInteger(1).intValue());
      assertEquals(1022, Primitive.createInteger(1022).intValue());
      assertEquals(1023, Primitive.createInteger(1023).intValue());
   }
}
