// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;


public class LookupHashMapTest extends HashTabTest
{
   /**
    * @see nexj.core.util.HashTab2DTest#init()
    */
   protected void init()
   {
      m_htab = new LookupHashMap();
   }

   /**
    * @see nexj.core.util.HashTabTest#testToString()
    */
   public void testToString()
   {
      assertEquals("{Key C=Value C, Key A=Value A, Key B=Value B}", m_htab.toString());
   }
}
