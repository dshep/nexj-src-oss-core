// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

public class SoftHashTabTest extends HashTabTest
{
   /**
    * @see nexj.core.util.HashTab2DTest#init()
    */
   protected void init()
   {
      m_htab = new SoftHashTab(1);
   }
}