// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.util.Properties;

import junit.framework.TestCase;

public class NetUtilTest extends TestCase
{
   public void testGetBindPort()
   {
      Properties prop = SysUtil.getConfigProperties();
      String sKey = "port.offset";
      String sValue = prop.getProperty(sKey);
      int nStep = 100; // use a large step to decrease test run time and interpolate pass

      try
      {
         // superuser reserved ports
         for (int i = -0x40f, nOffset = 1, nRange = 0x400 - nOffset,
              nNext = ((123 - nOffset + i) % nRange) + nOffset + nRange;
              i < 0x40f;
              i += nStep, nNext = ((nNext + nStep - nOffset) % nRange) + nOffset)
         {
            prop.setProperty(sKey, Integer.toString(i));
            assertEquals("Expected 123 + " + i + " = " + nNext, nNext, NetUtil.getBindPort(123));
         }

          // regular ports
         for (int i = -0x1ffff, nOffset = 0x400, nRange = 0x10000 - nOffset,
              nNext = ((12345 - nOffset + i) % nRange) + nOffset + nRange;
              i < 0x1ffff;
              i += nStep, nNext = ((nNext + nStep - nOffset) % nRange) + nOffset)
         {
            prop.setProperty(sKey, Integer.toString(i));
            assertEquals(
               "Expected 123456 + " + i + " = " + nNext, nNext, NetUtil.getBindPort(12345));
         }
      }
      finally
      {
         if (sValue == null)
         {
            prop.remove(sKey); // remove test value
         }
         else
         {
            prop.setProperty(sKey, sValue); // reset to original value
         }
      }
   }
}