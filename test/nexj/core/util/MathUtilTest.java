// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import junit.framework.TestCase;

public class MathUtilTest extends TestCase
{
   public void testCeil2()
   {
      assertEquals(0, MathUtil.ceil2(0));
      assertEquals(1, MathUtil.ceil2(1));
      assertEquals(2, MathUtil.ceil2(2));
      assertEquals(4, MathUtil.ceil2(3));
      assertEquals(8, MathUtil.ceil2(8));
      assertEquals(16, MathUtil.ceil2(11));
      assertEquals(32, MathUtil.ceil2(27));
      assertEquals(4096, MathUtil.ceil2(2567));
      assertEquals(32768, MathUtil.ceil2(31111));
   }
}
