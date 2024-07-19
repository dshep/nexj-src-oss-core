// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

public class WeakHashHolderTest extends HashHolderTest
{
   /**
    * @see nexj.core.util.HashHolderTest#init()
    */
   protected void init()
   {
      m_hh = new WeakHashHolder(1);
   }
}
