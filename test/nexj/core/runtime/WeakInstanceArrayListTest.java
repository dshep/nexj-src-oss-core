// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.runtime;

import nexj.core.runtime.InstanceList;

/**
 * Instance array list test with weak adding flags.
 */
public class WeakInstanceArrayListTest extends InstanceArrayListTest
{
   public WeakInstanceArrayListTest()
   {
      m_nFlags = InstanceList.WEAK;
   }
}
